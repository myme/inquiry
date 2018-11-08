{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

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
import           Data.Text (unpack, pack, toUpper, Text)
import           Data.Text.Zipper (clearZipper)
import qualified Graphics.Vty as V
import           Lens.Micro.Platform (over, set, view, makeLenses)

data EditMode = Normal | Insert deriving (Eq, Show)

data Method = GET | POST deriving (Show)

data Request = Request { _method :: Method
                       , _url :: Text
                       }

instance Show Request where
  show (Request m u) = show m <> " " <> unpack u

data AppState = AppState { _currentRequest :: Request
                         , _recentReqs :: [Request]
                         , _urlInput :: E.Editor Text Text
                         , _mode :: EditMode
                         } deriving (Show)

makeLenses ''Request
makeLenses ''AppState

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
          status = txt $ toUpper $ pack $ show $ view mode state

isKey :: V.Key -> BrickEvent n e -> Bool
isKey k (VtyEvent (V.EvKey k' _)) = k == k'
isKey _ _ = False

request :: AppState -> IO AppState
request state = do
  let req = view currentRequest state
  putStrLn $ "Running request: " <> show req
  _ <- getLine
  return $
    set (currentRequest . url) "" $
    over urlInput (E.applyEdit clearZipper) $
    over recentReqs (<> [req]) state

insertMap :: [(V.Key, AppState -> EventM n (Next AppState))]
insertMap = [(V.KEsc, M.continue . set mode Normal)
            ,(V.KEnter, M.suspendAndResume . request)
            ]

normalMap :: [(V.Key, AppState -> EventM n (Next AppState))]
normalMap = [(V.KChar 'i', M.continue . set mode Insert)
            ,(V.KChar 'q', M.halt)
            ]

onEvent :: AppState -> BrickEvent Text e -> EventM Text (Next AppState)
onEvent state (VtyEvent ev@(V.EvKey key _)) = do
  let action = case view mode state of
        Normal -> lookup key normalMap
        Insert -> lookup key insertMap <|> handleEditor
  case action of
    Nothing      -> M.continue state
    Just action' -> action' state
  where handleEditor = Just $ \s' -> do
          -- Perhaps handleEventLensed
          editor <- E.handleEditorEvent ev (view urlInput s')
          let input = foldr (<>) mempty $ E.getEditContents editor
          M.continue $
            set (currentRequest . url) input $
            set urlInput editor s'
onEvent state _ = M.continue state

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
