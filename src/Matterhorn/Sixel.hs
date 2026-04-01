{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Sixel image support for rendering inline images in the terminal.
-- Converts PNG/GIF image data to Sixel escape sequences suitable for
-- terminals that support the Sixel graphics protocol (e.g. Foot,
-- xterm -ti vt340, mlterm).
module Matterhorn.Sixel
  ( ImageCache
  , newImageCache
  , lookupOrFetchEmojiImage
  , pngToSixel
  , queryCellPixelSize
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import qualified Control.Exception as E
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.IORef as IORef
import qualified Data.Map.Strict as Map
import           Data.Text ( Text )
import qualified Data.Text as T
import           System.Exit ( ExitCode(..) )
import           System.IO ( hClose )
import           System.Process
                   ( CreateProcess(..), StdStream(..), proc
                   , createProcess, waitForProcess
                   , readProcessWithExitCode
                   )

import           Network.Mattermost.Types ( Session )
import qualified Network.Mattermost.Endpoints as MM

-- | Cache mapping emoji name -> Maybe sixel bytestring.
-- Nothing means we tried and failed to fetch/convert this emoji.
data ImageCache = ImageCache
    { icCache :: IORef.IORef (Map.Map Text (Maybe BS.ByteString))
    , icSession :: Session
    , icCellWidth :: Int
    , icCellHeight :: Int
    }

-- | Create a new empty image cache. The cell dimensions are used to
-- scale images to fit in terminal cells.
newImageCache :: Session -> Int -> Int -> IO ImageCache
newImageCache session cw ch = do
    ref <- IORef.newIORef Map.empty
    return $ ImageCache ref session cw ch

-- | Look up a custom emoji image in the cache, fetching and converting
-- it if not already cached. Returns the Sixel bytestring if available.
lookupOrFetchEmojiImage :: ImageCache -> Text -> IO (Maybe BS.ByteString)
lookupOrFetchEmojiImage ic emojiName = do
    cache <- IORef.readIORef (icCache ic)
    case Map.lookup emojiName cache of
        Just result -> return result
        Nothing -> do
            result <- fetchAndConvert ic emojiName
            IORef.modifyIORef' (icCache ic) (Map.insert emojiName result)
            return result

fetchAndConvert :: ImageCache -> Text -> IO (Maybe BS.ByteString)
fetchAndConvert ic emojiName = do
    result <- E.try $ fetchEmojiByName ic emojiName
    case result of
        Left (_ :: E.SomeException) -> return Nothing
        Right Nothing -> return Nothing
        Right (Just pngData) -> do
            -- Scale to 2 cells wide x 1 cell tall
            let targetW = icCellWidth ic * 2
                targetH = icCellHeight ic
            sixelResult <- pngToSixel pngData targetW targetH
            case sixelResult of
                Left _err -> return Nothing
                Right sixelData -> return (Just sixelData)

-- | Fetch the image for a custom emoji by name. First searches for the
-- emoji to get its ID, then fetches the image data.
fetchEmojiByName :: ImageCache -> Text -> IO (Maybe BS.ByteString)
fetchEmojiByName ic name = do
    emojis <- MM.mmSearchCustomEmoji name (icSession ic)
    case filter (\e -> MM.emojiName e == name) emojis of
        (emoji:_) -> do
            imgData <- MM.mmGetCustomEmojiImage (MM.emojiId emoji) (icSession ic)
            return (Just imgData)
        [] -> return Nothing

-- | Convert raw image bytes (PNG/GIF) to Sixel format using img2sixel.
-- The target width and height are in pixels.
pngToSixel :: BS.ByteString
           -- ^ Raw image data (PNG, GIF, etc.)
           -> Int
           -- ^ Target width in pixels
           -> Int
           -- ^ Target height in pixels
           -> IO (Either String BS.ByteString)
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
-- Returns (cellWidth, cellHeight) in pixels, or a default if the query
-- fails.
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
