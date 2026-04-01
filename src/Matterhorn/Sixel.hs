{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Sixel image support for rendering inline images in the terminal.
-- Converts PNG/GIF image data to Sixel escape sequences suitable for
-- terminals that support the Sixel graphics protocol (e.g. Foot,
-- xterm -ti vt340, mlterm).
--
-- Emoji images are fetched asynchronously: the first render of a custom
-- emoji shows @:name:@ text, kicks off a background fetch, and
-- subsequent renders show the Sixel image once it's ready.
module Matterhorn.Sixel
  ( ImageCache
  , newImageCache
  , lookupOrFetchEmojiImage
  , lookupSixelEmoji
  , pngToSixel
  , queryCellPixelSize
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Control.Concurrent ( forkIO )
import qualified Control.Exception as E
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
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

-- | Tri-state cache entry for emoji images.
data CacheEntry
    = Pending
    -- ^ Fetch is in progress.
    | Ready (Maybe BS.ByteString)
    -- ^ Fetch completed. Nothing means the emoji could not be fetched
    -- or converted (permanently cached as a failure).
    deriving (Eq)

-- | Cache for custom emoji Sixel images. Fetches happen on background
-- threads so the UI is never blocked.
data ImageCache = ImageCache
    { icCache :: IORef.IORef (Map.Map Text CacheEntry)
    , icSession :: Session
    , icCellWidth :: Int
    , icCellHeight :: Int
    }

-- | Create a new empty image cache.
newImageCache :: Session -> Int -> Int -> IO ImageCache
newImageCache session cw ch = do
    ref <- IORef.newIORef Map.empty
    return $ ImageCache ref session cw ch

-- | Look up a custom emoji Sixel image. Returns:
--
-- * @Just bs@ — Sixel data is ready.
-- * @Nothing@ — Not ready yet (first call triggers async fetch) or
--   permanently unavailable.
--
-- This function never blocks on network I/O. Safe to call from
-- rendering code (including via unsafePerformIO).
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

-- | Look up a custom emoji's Sixel image from the cache. Uses
-- unsafePerformIO since the ImageCache is a memoized IORef lookup.
-- On cache miss, kicks off an async fetch and returns Nothing
-- (the emoji renders as :name: text until the fetch completes).
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
        Right (Just pngData) -> do
            let targetW = icCellWidth ic * 2
                targetH = icCellHeight ic
            sixelResult <- pngToSixel pngData targetW targetH
            case sixelResult of
                Left _err -> return Nothing
                Right sixelData -> return (Just sixelData)

-- | Fetch the image for a custom emoji by name.
fetchEmojiByName :: ImageCache -> Text -> IO (Maybe BS.ByteString)
fetchEmojiByName ic name = do
    emojis <- MM.mmSearchCustomEmoji name (icSession ic)
    case filter (\e -> MM.emojiName e == name) emojis of
        (emoji:_) -> do
            imgData <- MM.mmGetCustomEmojiImage (MM.emojiId emoji) (icSession ic)
            return (Just imgData)
        [] -> return Nothing

-- | Convert raw image bytes (PNG/GIF) to Sixel format using img2sixel.
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
