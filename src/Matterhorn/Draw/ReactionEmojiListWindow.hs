module Matterhorn.Draw.ReactionEmojiListWindow
  ( drawReactionEmojiListWindow
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick
import           Brick.Widgets.List ( listSelectedFocusedAttr )
import qualified Data.Text as T

import           Network.Mattermost.Types ( TeamId )

import           Matterhorn.Draw.ListWindow ( drawListWindow, WindowPosition(..) )
import           Matterhorn.Emoji ( EmojiCollection, lookupEmojiUnicode )
import           Matterhorn.Types
import           Matterhorn.Themes


drawReactionEmojiListWindow :: ChatState -> TeamId -> Widget Name
drawReactionEmojiListWindow st tId =
    let em = st^.csResources.crEmoji
        window = drawListWindow (st^.csTeam(tId).tsReactionEmojiListWindow)
                                  (const $ txt "Search Emoji")
                                  (const $ txt "No matching emoji found.")
                                  (const $ txt "Search emoji:")
                                  (renderEmoji em)
                                  Nothing
                                  WindowCenter
                                  80
    in joinBorders window

renderEmoji :: EmojiCollection -> Bool -> (Bool, T.Text) -> Widget Name
renderEmoji em sel (mine, e) =
    let maybeForce = if sel
                     then forceAttr listSelectedFocusedAttr
                     else id
        emojiDisplay = case lookupEmojiUnicode em e of
            Just unicode -> unicode <> " :" <> e <> ":"
            Nothing      -> ":" <> e <> ":"
    in clickable (ClickableReactionEmojiListWindowEntry (mine, e)) $
       maybeForce $
       padRight Max $
       hBox [ if mine then txt " * " else txt "   "
            , withDefAttr emojiAttr $ txt emojiDisplay
            ]
