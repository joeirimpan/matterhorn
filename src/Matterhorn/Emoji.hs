{-# LANGUAGE ScopedTypeVariables #-}
module Matterhorn.Emoji
  ( EmojiCollection
  , loadEmoji
  , emptyEmojiCollection
  , getMatchingEmoji
  , matchesEmoji
  , lookupEmojiUnicode
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import qualified Control.Exception as E
import           Control.Monad.Except ( MonadError(..), ExceptT(..), runExceptT )
import           Control.Monad.Trans ( MonadTrans(..) )
import           Data.Char ( chr )
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Sequence as Seq
import           Numeric ( readHex )

import           Network.Mattermost.Types ( Session )
import qualified Network.Mattermost.Endpoints as MM


data EmojiEntry = EmojiEntry
    { eeShortNames :: Seq.Seq T.Text
    , eeUnified    :: T.Text
    }

newtype EmojiData = EmojiData (Seq.Seq EmojiEntry)

-- | The collection of all emoji names we loaded from a JSON disk file,
-- plus a mapping from shortname to Unicode text for rendering.
--
-- You might rightly ask: why don't we use a Trie here, for efficient
-- lookups? The answer is that we need infix lookups; prefix matches are
-- not enough. In practice it seems not to matter that much; despite the
-- O(n) search we get good enough performance that we aren't worried
-- about this. If at some point this becomes an issue, other data
-- structures with good infix lookup performance should be identified
-- (full-text search, perhaps?).
data EmojiCollection = EmojiCollection
    { ecNames      :: [T.Text]
    , ecUnicodeMap :: Map.Map T.Text T.Text
    }

instance A.FromJSON EmojiData where
    parseJSON = A.withArray "EmojiData" $ \v -> do
        entries <- forM v $ \val ->
            flip (A.withObject "EmojiData Entry") val $ \obj -> do
                as <- obj A..: "short_names"
                names <- forM as $ A.withText "Alias list element" return
                unified <- obj A..: "unified"
                return $ EmojiEntry names unified

        return $ EmojiData $ mconcat $ fmap Seq.singleton $ F.toList entries

emptyEmojiCollection :: EmojiCollection
emptyEmojiCollection = EmojiCollection mempty Map.empty

-- | Convert a unified codepoint string like "1F600" or "1F1FA-1F1F8"
-- to the corresponding Unicode text.
unifiedToText :: T.Text -> T.Text
unifiedToText t =
    T.pack $ concatMap codePointToChars $ T.splitOn "-" t
  where
    codePointToChars :: T.Text -> String
    codePointToChars cp = case readHex (T.unpack cp) of
        [(n, "")] -> [chr n]
        _         -> []

-- | Load an EmojiCollection from a JSON disk file.
loadEmoji :: FilePath -> IO (Either String EmojiCollection)
loadEmoji path = runExceptT $ do
    result <- lift $ E.try $ BSL.readFile path
    case result of
        Left (e::E.SomeException) -> throwError $ show e
        Right bs -> do
            EmojiData entries <- ExceptT $ return $ A.eitherDecode bs
            let allNames = concatMap (\e -> F.toList $ eeShortNames e) $ F.toList entries
                unicodeMap = Map.fromList
                    [ (T.toLower name, unifiedToText (eeUnified entry))
                    | entry <- F.toList entries
                    , name  <- F.toList (eeShortNames entry)
                    ]
            return $ EmojiCollection
                { ecNames      = T.toLower <$> allNames
                , ecUnicodeMap = unicodeMap
                }

-- | Look up the Unicode text for a given emoji shortname.
-- Returns Nothing for custom (server-side) emoji that have no Unicode
-- equivalent.
lookupEmojiUnicode :: EmojiCollection -> T.Text -> Maybe T.Text
lookupEmojiUnicode ec name =
    Map.lookup (T.toLower name) (ecUnicodeMap ec)

-- | Look up matching emoji in the collection using the provided search
-- string. This does a case-insensitive infix match. The search string
-- may be provided with or without leading and trailing colons.
lookupEmoji :: EmojiCollection -> T.Text -> [T.Text]
lookupEmoji ec search =
    filter (matchesEmoji search) (ecNames ec)

-- | Match a search string against an emoji.
matchesEmoji :: T.Text
             -- ^ The search string (will be converted to lowercase and
             -- colons will be removed)
             -> T.Text
             -- ^ The emoji string (assumed to be lowercase and without
             -- leading/trailing colons)
             -> Bool
matchesEmoji searchString e =
    sanitizeEmojiSearch searchString `T.isInfixOf` e

sanitizeEmojiSearch :: T.Text -> T.Text
sanitizeEmojiSearch = stripColons . T.toLower . T.strip

-- | Perform an emoji search against both the local EmojiCollection as
-- well as the server's custom emoji. Return the results, sorted. If the
-- empty string is specified, all local and all custom emoji will be
-- included in the returned list.
getMatchingEmoji :: Session -> EmojiCollection -> T.Text -> IO [T.Text]
getMatchingEmoji session em rawSearchString = do
    let localAlts = lookupEmoji em rawSearchString
        sanitized = sanitizeEmojiSearch rawSearchString
    customResult <- E.try $ case T.null sanitized of
        True -> MM.mmGetListOfCustomEmoji Nothing Nothing session
        False -> MM.mmSearchCustomEmoji sanitized session

    let custom = case customResult of
            Left (_::E.SomeException) -> []
            Right result -> result

    return $ sort $ (MM.emojiName <$> custom) <> localAlts

stripColons :: T.Text -> T.Text
stripColons t =
    stripHeadColon $ stripTailColon t
    where
        stripHeadColon v = if ":" `T.isPrefixOf` v
                           then T.tail v
                           else v
        stripTailColon v = if ":" `T.isSuffixOf` v
                           then T.init v
                           else v
