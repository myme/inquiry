{-# LANGUAGE OverloadedStrings #-}

module Inquiry
    ( app
    ) where

import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import           Brick.Types (Padding(..), BrickEvent(..), Next, EventM, BrickEvent, Widget)
import           Brick.Widgets.Border (border)
import           Brick.Widgets.Border.Style (unicode)
import           Brick.Widgets.Center (hCenter, center)
import           Brick.Widgets.Core (txt, (<+>), emptyWidget, padRight, str, withBorderStyle, (<=>))
import qualified Brick.Widgets.Edit as E
import           Control.Applicative ((<|>))
import           Control.Monad (void)
import           Data.Text (Text)
import qualified Graphics.Vty as V
import           Inquiry.Commands (continue, request, insertMode, exMode, normalMode, quit)
import           Inquiry.Types (url, currentRequest, AppState(..), EditMode(..), Method(..), Request(..), mode, recentReqs, urlInput)
import           Lens.Micro.Platform (view, set)

mainArea :: [Request] -> Widget a
mainArea [] = center $ str "No recent requests!"
mainArea recent = center $
  str "Recent requests:" <=> foldr ((<=>) . str . show) emptyWidget recent

drawUI :: AppState -> [Widget Text]
drawUI state = [ui]
    where ui = withBorderStyle unicode
             $ title <=> border input <=> mainArea (view recentReqs state) <=> status
          title = str " " <=> hCenter (str "inQuiry")
          inInsert = view mode state == Insert
          editor = E.renderEditor (foldr ((<+>) . txt) emptyWidget) inInsert (view urlInput state)
          input = padRight Max editor
          status = case view mode state of
            Ex -> txt ":"
            Insert -> txt "-- INSERT --"
            Normal -> txt " "

exMap :: [(V.Key, AppState -> EventM n (Next AppState))]
exMap = [(V.KEsc, normalMode)
        ,(V.KChar 'q', quit)
        ]

insertMap :: [(V.Key, AppState -> EventM n (Next AppState))]
insertMap = [(V.KEsc, normalMode)
            ,(V.KEnter, request)
            ]

normalMap :: [(V.Key, AppState -> EventM n (Next AppState))]
normalMap = [(V.KChar ':', exMode)
            ,(V.KChar 'i', insertMode)
            ]

onEvent :: AppState -> BrickEvent Text e -> EventM Text (Next AppState)
onEvent state (VtyEvent ev@(V.EvKey key _)) = do
  let action = case view mode state of
        Ex     -> lookup key exMap
        Normal -> lookup key normalMap
        Insert -> lookup key insertMap <|> handleEditor
  case action of
    Nothing      -> M.continue state
    Just action' -> action' state
  where handleEditor = Just $ \s' -> do
          -- TODO: Perhaps handleEventLensed
          editor <- E.handleEditorEvent ev (view urlInput s')
          let input = foldr (<>) mempty $ E.getEditContents editor
          continue $
            set (currentRequest . url) input $
            set urlInput editor s'
onEvent state _ = continue state

app :: IO ()
app = void $ M.defaultMain app' initialState
  where app' = M.App
          { M.appAttrMap = const $ A.attrMap V.defAttr []
          , M.appChooseCursor = M.showFirstCursor
          , M.appDraw = drawUI
          , M.appHandleEvent = onEvent
          , M.appStartEvent = return
          }
        initialState = AppState
          { _currentRequest = Request GET "http://"
          , _mode = Insert
          , _recentReqs = []
          , _urlInput = E.editorText "urlInput" (Just 1) ""
          }
