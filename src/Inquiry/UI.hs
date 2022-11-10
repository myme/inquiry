{-# LANGUAGE OverloadedStrings #-}

module Inquiry.UI
  ( drawUI,
  )
where

import Brick.Types (Padding (..), Widget)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center (center, centerLayer)
import Brick.Widgets.Core (emptyWidget, padBottom, padLeftRight, padRight, padTopBottom, str, txt, withBorderStyle, (<+>), (<=>))
import qualified Brick.Widgets.Edit as E
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack)
import Inquiry.Request (Method (..), Request (..), RequestHistory, Response, reqMethod, reqUrl, resBody)
import Inquiry.Types (AppState, EditMode (..), mode, requestHistory, showRecents, urlInput)
import Inquiry.Zipper (emptyZipper, focus)
import Lens.Micro.Platform ((^.))

drawRecents :: RequestHistory -> Widget a
drawRecents recent =
  centerLayer $
    borderWithLabel (txt "Recent requests") $
      padTopBottom 5 $
        padLeftRight
          10
          recents'
  where
    recents'
      | recent == emptyZipper = txt "No recent requests!"
      | otherwise = foldr ((<=>) . str . reqRepr) emptyWidget list
    list = reverse $ toList recent
    reqRepr (r, _) = show (r ^. reqMethod) <> " " <> unpack (r ^. reqUrl)

drawResponse :: Maybe Response -> Widget a
drawResponse response' = borderWithLabel (txt "Response") $ padRight Max $ padBottom Max text
  where
    text = case response' of
      Nothing -> center $ txt "No response"
      Just response'' -> txt (response'' ^. resBody)

drawUI :: AppState -> [Widget Text]
drawUI state = [recents, main]
  where
    (request, response) = fromMaybe (Request GET "", Nothing) $ state ^. requestHistory . focus
    recents =
      if state ^. showRecents
        then drawRecents (state ^. requestHistory)
        else emptyWidget
    main =
      withBorderStyle unicode $
        navigator
          <=> headers
          <=> drawResponse response
          <=> status
    navigator = borderWithLabel (txt "inQuiry") $ method <+> padRight Max editor
    inInsert = state ^. mode == Insert
    editor = E.renderEditor (foldr ((<+>) . txt) emptyWidget) inInsert (state ^. urlInput)
    method = str $ show (request ^. reqMethod) <> ": "
    headers = borderWithLabel (txt "Headers") $ padRight Max $ txt "Empty..."
    status = case state ^. mode of
      Ex -> txt ":"
      Insert -> txt "-- INSERT --"
      Normal -> txt " "
