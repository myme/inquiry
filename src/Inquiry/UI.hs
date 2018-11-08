{-# LANGUAGE OverloadedStrings #-}

module Inquiry.UI
  ( drawUI
  ) where

import           Brick.Types (Padding(..), Widget)
import           Brick.Widgets.Border (border)
import           Brick.Widgets.Border.Style (unicode)
import           Brick.Widgets.Center (hCenter, center)
import           Brick.Widgets.Core (txt, (<+>), emptyWidget, padRight, str, withBorderStyle, (<=>))
import qualified Brick.Widgets.Edit as E
import           Data.Foldable (toList)
import           Data.Text (Text)
import           Inquiry.Types (mode, urlInput, requestHistory, AppState, EditMode(..), Request)
import           Inquiry.Zipper (Zipper, emptyZipper)
import           Lens.Micro.Platform (view)

mainArea :: Zipper Request -> Widget a
mainArea recent | recent == emptyZipper = center $ txt "No recent requests!"
                | otherwise = center $
                    txt "Recent requests:" <=> foldr ((<=>) . str . show) emptyWidget (reverse $ toList recent)

drawUI :: AppState -> [Widget Text]
drawUI state = [ui]
    where ui = withBorderStyle unicode
             $ title <=> border input <=> mainArea (view requestHistory state) <=> status
          title = txt " " <=> hCenter (txt "inQuiry")
          inInsert = view mode state == Insert
          editor = E.renderEditor (foldr ((<+>) . txt) emptyWidget) inInsert (view urlInput state)
          input = padRight Max editor
          status = case view mode state of
            Ex -> txt ":"
            Insert -> txt "-- INSERT --"
            Normal -> txt " "
