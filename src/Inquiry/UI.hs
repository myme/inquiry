{-# LANGUAGE OverloadedStrings #-}

module Inquiry.UI
  ( drawUI
  ) where

import           Brick.Types (Padding(..), Widget)
import           Brick.Widgets.Border (borderWithLabel)
import           Brick.Widgets.Border.Style (unicode)
import           Brick.Widgets.Center (centerLayer, center)
import           Brick.Widgets.Core (padBottom, padTopBottom, padLeftRight, txt, (<+>), emptyWidget, padRight, str, withBorderStyle, (<=>))
import qualified Brick.Widgets.Edit as E
import           Data.Foldable (toList)
import           Data.Text (Text)
import           Inquiry.Types (showRecents, response, currentMethod, mode, urlInput, requestHistory, AppState, EditMode(..), Request)
import           Inquiry.Zipper (Zipper, emptyZipper)
import           Lens.Micro.Platform ((^.))

drawRecents :: Zipper Request -> Widget a
drawRecents recent = centerLayer $
    borderWithLabel (txt "Recent requests") $
    padTopBottom 5 $
    padLeftRight 10
    recents'
  where recents' | recent == emptyZipper = txt "No recent requests!"
                 | otherwise = foldr ((<=>) . str . show) emptyWidget list
        list = reverse $ toList recent

drawResponse :: Text -> Widget a
drawResponse response' = borderWithLabel (txt "Response") $ padRight Max $ padBottom Max text
  where text = if response' == "" then center $ txt "No response" else txt response'

drawUI :: AppState -> [Widget Text]
drawUI state = [recents, main]
    where recents = if state ^. showRecents
            then drawRecents (state ^. requestHistory)
            else emptyWidget
          main = withBorderStyle unicode $
            navigator <=>
            headers <=>
            drawResponse (state ^. response) <=>
            status
          navigator = borderWithLabel (txt "inQuiry") $ method <+> padRight Max editor
          inInsert = state ^. mode == Insert
          editor = E.renderEditor (foldr ((<+>) . txt) emptyWidget) inInsert (state ^. urlInput)
          method = str $ show (state ^. currentMethod) <> ": "
          headers = borderWithLabel (txt "Headers") $ padRight Max $ txt "Empty..."
          status = case state ^. mode of
            Ex     -> txt ":"
            Insert -> txt "-- INSERT --"
            Normal -> txt " "
