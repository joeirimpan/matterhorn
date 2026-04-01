{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Inline image support for rendering custom emoji in the terminal.
-- Supports Sixel (Foot, xterm, mlterm, WezTerm) and Kitty graphics
-- protocol (Foot 1.14+, Kitty, Ghostty, WezTerm).
--
-- Emoji images are fetched asynchronously: the first render of a custom
-- emoji shows @:name:@ text, kicks off a background fetch, and
-- subsequent renders show the inline image once it's ready.
module Matterhorn.Sixel
  ( ImageCache
  , ImageProtocol(..)
  , newImageCache
  , lookupOrFetchEmojiImage
  , lookupSixelEmoji
  , pngToSixel
  , pngToKittyGraphics
  , queryCellPixelSize
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Control.Concurrent ( forkIO )
import qualified Control.Exception as E
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Base64 as B64
import qualified Data.IORef as IORef
import qualified Data.Map.Strict as Map
import           Data.Text ( Text )
import qualified Data.Text as T
import           System.Exit ( ExitCode(..) )
import           System.IO ( hClose )
import           System.IO.Unsafe ( unsafePerformIO )
import           System.Process
                   ( CreateProcess(..), StdStream(..), proc
                   , createProcess, waitForProcess
                   , readProcessWithExitCode
                   )

import           Network.Mattermost.Types ( Session )
import qualified Network.Mattermost.Endpoints as MM

-- | The image protocol for rendering inline custom emoji.
data ImageProtocol =
    NoImageProtocol
    -- ^ Disable inline images; custom emoji render as @:name:@ text.
    | SixelProtocol
    -- ^ Use the Sixel graphics protocol (requires @img2sixel@).
    | KittyProtocol
    -- ^ Use the Kitty graphics protocol.
    deriving (Eq, Show)

-- | Tri-state cache entry for emoji images.
data CacheEntry
    = Pending
    -- ^ Fetch is in progress.
    | Ready (Maybe BS.ByteString)
    -- ^ Fetch completed. Nothing means the emoji could not be fetched
    -- or converted (permanently cached as a failure).
    deriving (Eq)

-- | Cache for custom emoji inline images. Fetches happen on background
-- threads so the UI is never blocked.
data ImageCache = ImageCache
    { icCache :: IORef.IORef (Map.Map Text CacheEntry)
    , icSession :: Session
    , icProtocol :: ImageProtocol
    , icCellWidth :: Int
    , icCellHeight :: Int
    }

-- | Create a new empty image cache.
newImageCache :: Session -> ImageProtocol -> Int -> Int -> IO ImageCache
newImageCache session proto cw ch = do
    ref <- IORef.newIORef Map.empty
    return $ ImageCache ref session proto cw ch

-- | Look up a custom emoji inline image. Returns:
--
-- * @Just bs@ — image data is ready (Sixel or Kitty escape sequence).
-- * @Nothing@ — not ready yet (first call triggers async fetch) or
--   permanently unavailable.
--
-- This function never blocks on network I/O.
lookupOrFetchEmojiImage :: ImageCache -> Text -> IO (Maybe BS.ByteString)
lookupOrFetchEmojiImage ic emojiName = do
    cache <- IORef.readIORef (icCache ic)
    case Map.lookup emojiName cache of
        Just (Ready result) -> return result
        Just Pending        -> return Nothing
        Nothing -> do
            IORef.modifyIORef' (icCache ic) (Map.insert emojiName Pending)
            _ <- forkIO $ do
                result <- fetchAndConvert ic emojiName
                IORef.modifyIORef' (icCache ic)
                    (Map.insert emojiName (Ready result))
            return Nothing

-- | Look up a custom emoji's inline image from the cache.
-- On cache miss, kicks off an async fetch and returns Nothing.
{-# NOINLINE lookupSixelEmoji #-}
lookupSixelEmoji :: Text -> ImageCache -> Maybe BS.ByteString
lookupSixelEmoji emojiName ic =
    unsafePerformIO $ lookupOrFetchEmojiImage ic emojiName

fetchAndConvert :: ImageCache -> Text -> IO (Maybe BS.ByteString)
fetchAndConvert ic emojiName = do
    result <- E.try $ fetchEmojiByName ic emojiName
    case result of
        Left (_ :: E.SomeException) -> return Nothing
        Right Nothing -> return Nothing
        Right (Just imgData) -> do
            let targetW = icCellWidth ic * 2
                targetH = icCellHeight ic
            convertResult <- case icProtocol ic of
                SixelProtocol -> pngToSixel imgData targetW targetH
                KittyProtocol -> do
                    -- Kitty only accepts PNG. Convert any image format
                    -- (GIF, JPEG, etc.) to PNG first.
                    pngResult <- ensurePng imgData targetW targetH
                    case pngResult of
                        Left _err -> return $ Left _err
                        Right pngBytes -> return $ Right $ pngToKittyGraphics pngBytes
                NoImageProtocol -> return $ Left "images disabled"
            case convertResult of
                Left _err -> return Nothing
                Right out -> return (Just out)

-- | Fetch the image for a custom emoji by name.
fetchEmojiByName :: ImageCache -> Text -> IO (Maybe BS.ByteString)
fetchEmojiByName ic name = do
    emojis <- MM.mmSearchCustomEmoji name (icSession ic)
    case filter (\e -> MM.emojiName e == name) emojis of
        (emoji:_) -> do
            imgData <- MM.mmGetCustomEmojiImage (MM.emojiId emoji) (icSession ic)
            return (Just imgData)
        [] -> return Nothing

-- | Convert any image format (GIF, JPEG, etc.) to PNG at the target
-- size using ImageMagick. If the input is already a correctly-sized
-- PNG, this is a no-op in terms of quality (but still re-encodes).
ensurePng :: BS.ByteString -> Int -> Int -> IO (Either String BS.ByteString)
ensurePng imgData targetW targetH = do
    let geometry = show targetW ++ "x" ++ show targetH ++ "!"
    result <- E.try $ readProcessBS "magick"
                [ "-"               -- read from stdin
                , "-resize", geometry
                , "-background", "none"
                , "-flatten"
                , "png:-"           -- write PNG to stdout
                ]
                imgData
    case result of
        Left (e :: E.SomeException) ->
            -- Fall back to 'convert' for older ImageMagick
            do result2 <- E.try $ readProcessBS "convert"
                            [ "-"
                            , "-resize", geometry
                            , "-background", "none"
                            , "-flatten"
                            , "png:-"
                            ]
                            imgData
               case result2 of
                   Left (e2 :: E.SomeException) ->
                       return $ Left $ "image conversion failed: " ++ show e2
                   Right (ExitSuccess, out, _) -> return $ Right out
                   Right (ExitFailure n, _, err) ->
                       return $ Left $ "convert failed (" ++ show n ++ "): " ++ BS8.unpack err
        Right (ExitSuccess, out, _) -> return $ Right out
        Right (ExitFailure n, _, err) ->
            return $ Left $ "magick failed (" ++ show n ++ "): " ++ BS8.unpack err

-- | Convert PNG bytes to Kitty graphics protocol escape sequence.
-- The PNG data is base64-encoded and wrapped in Kitty's APC sequence.
pngToKittyGraphics :: BS.ByteString
                   -- ^ PNG image data
                   -> BS.ByteString
pngToKittyGraphics pngData =
    -- Kitty graphics protocol:
    -- ESC _ G <key>=<value>,... ; <base64 data> ESC \.
    --
    -- a=T  : transmit and display
    -- f=100: PNG format
    -- t=d  : direct (data is in the payload)
    -- c=2,r=1: display in 2 columns, 1 row of cells
    -- C=1  : do not move cursor (we handle cursor positioning in vty)
    -- q=2  : suppress response from terminal
    --
    -- For large payloads we must chunk into 4096-byte pieces.
    let b64 = B64.encode pngData
        header = "\x1b_Ga=T,f=100,t=d,c=2,r=1,C=1,q=2"
    in if BS.length b64 <= 4096
        then header <> ";" <> b64 <> "\x1b\\"
        else emitChunked header b64

-- | Emit Kitty graphics data in chunks of 4096 bytes.
-- First chunk uses the full header with m=1 (more data follows).
-- Middle chunks use m=1. Last chunk uses m=0 (no more data).
emitChunked :: BS.ByteString -> BS.ByteString -> BS.ByteString
emitChunked header b64 = go True b64
  where
    go isFirst remaining =
        let (chunk, rest) = BS.splitAt 4096 remaining
            hasMore = not (BS.null rest)
            prefix = if isFirst
                     then header <> ",m=" <> if hasMore then "1;" else "0;"
                     else "\x1b_Gm=" <> if hasMore then "1;" else "0;"
            thisChunk = prefix <> chunk <> "\x1b\\"
        in if hasMore
           then thisChunk <> go False rest
           else thisChunk

-- | Convert raw image bytes to Sixel format using img2sixel.
pngToSixel :: BS.ByteString -> Int -> Int -> IO (Either String BS.ByteString)
pngToSixel imageData targetW targetH = do
    result <- E.try $ readProcessBS "img2sixel"
                [ "-w", show targetW
                , "-h", show targetH
                , "-"
                ]
                imageData
    case result of
        Left (e :: E.SomeException) -> return $ Left (show e)
        Right (ExitSuccess, out, _) -> return $ Right out
        Right (ExitFailure n, _, err) ->
            return $ Left $ "img2sixel failed (" ++ show n ++ "): " ++ BS8.unpack err

-- | Run a process with ByteString stdin/stdout.
readProcessBS :: FilePath -> [String] -> BS.ByteString -> IO (ExitCode, BS.ByteString, BS.ByteString)
readProcessBS cmd args input = do
    let cp = (proc cmd args)
                { std_in  = CreatePipe
                , std_out = CreatePipe
                , std_err = CreatePipe
                }
    (Just hin, Just hout, Just herr, ph) <- createProcess cp
    BS.hPut hin input
    hClose hin
    out <- BS.hGetContents hout
    err <- BS.hGetContents herr
    ec <- waitForProcess ph
    return (ec, out, err)

-- | Query the terminal for cell pixel dimensions using TIOCGWINSZ.
queryCellPixelSize :: IO (Int, Int)
queryCellPixelSize = do
    result <- E.try $ do
        (ec, out, _) <- readProcessWithExitCode "python3"
            [ "-c"
            , "import fcntl,struct,termios,sys; " ++
              "r=fcntl.ioctl(sys.stdout.fileno(),termios.TIOCGWINSZ,b'\\0'*8); " ++
              "rows,cols,xpix,ypix=struct.unpack('HHHH',r); " ++
              "print(xpix//cols,ypix//rows)"
            ] ""
        case ec of
            ExitSuccess -> case words out of
                [cw, ch] -> return (read cw :: Int, read ch :: Int)
                _ -> return (8, 16)
            _ -> return (8, 16)
    case result of
        Left (_ :: E.SomeException) -> return (8, 16)
        Right dims -> return dims
