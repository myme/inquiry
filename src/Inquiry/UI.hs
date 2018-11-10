{-# LANGUAGE OverloadedStrings #-}

module Inquiry.UI
  ( drawUI
  ) where

import           Brick.Types (Padding(..), Widget)
import           Brick.Widgets.Border (borderWithLabel)
import           Brick.Widgets.Border.Style (unicode)
import           Brick.Widgets.Center (center)
import           Brick.Widgets.Core (txt, (<+>), emptyWidget, padRight, str, withBorderStyle, (<=>))
import qualified Brick.Widgets.Edit as E
import           Data.Foldable (toList)
import           Data.Text (Text)
import           Inquiry.Types (currentMethod, mode, urlInput, requestHistory, AppState, EditMode(..), Request)
import           Inquiry.Zipper (Zipper, emptyZipper)
import           Lens.Micro.Platform ((^.))

recents :: Zipper Request -> Widget a
recents recent = borderWithLabel (txt "Recent requests") recents'
  where recents' | recent == emptyZipper = center $ txt "No recent requests!"
                 | otherwise = center $ foldr ((<=>) . str . show) emptyWidget list
        list = reverse $ toList recent

drawUI :: AppState -> [Widget Text]
drawUI state = [ui]
    where ui = withBorderStyle unicode
             $ navigator <=> headers <=> recents (state ^. requestHistory) <=> status
          navigator = borderWithLabel (txt "inQuiry") $ method <+> padRight Max editor
          inInsert = state ^. mode == Insert
          editor = E.renderEditor (foldr ((<+>) . txt) emptyWidget) inInsert (state ^. urlInput)
          method = str $ show (state ^. currentMethod) <> ": "
          headers = borderWithLabel (txt "Headers") $ padRight Max $ txt "Empty..."
          status = case state ^. mode of
            Ex     -> txt ":"
            Insert -> txt "-- INSERT --"
            Normal -> txt " "
