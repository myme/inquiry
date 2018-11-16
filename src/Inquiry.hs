-- | Main Inquiry library entry point

{-# LANGUAGE OverloadedStrings #-}

module Inquiry
    ( app
    ) where

import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import           Brick.Types (BrickEvent(..), Next, EventM, BrickEvent)
import qualified Brick.Widgets.Edit as E
import           Control.Applicative ((<|>))
import           Control.Monad (void)
import           Data.Text (Text)
import qualified Graphics.Vty as V
import           Inquiry.Commands (prevHistoryItem, continue, cycleMethod, request, insertMode, exMode, normalMode, nextHistoryItem, quit, toggleRecents)
import           Inquiry.Input (input)
import           Inquiry.Types (AppState(..), EditMode(..), Method(..), mode, urlInput)
import           Inquiry.UI (drawUI)
import           Inquiry.Zipper (emptyZipper)
import           Lens.Micro.Platform (view, set)

-- | Ex (Command) mode keymap
exMap :: [(V.Key, AppState -> EventM n (Next AppState))]
exMap = [(V.KEsc, normalMode)
        ,(V.KChar 'q', quit)
        ]

-- | Insert mode keymap
insertMap :: [(V.Key, AppState -> EventM n (Next AppState))]
insertMap = [(V.KEsc, normalMode)
            ,(V.KEnter, request)
            ]

-- | Normal mode keymap
normalMap :: [(V.Key, AppState -> EventM n (Next AppState))]
normalMap = [(V.KChar ':', exMode)
            ,(V.KChar 'i', insertMode)
            ,(V.KChar 'q', quit)
            ,(V.KChar 'm', cycleMethod)
            ,(V.KChar 'n', nextHistoryItem)
            ,(V.KChar 'p', prevHistoryItem)
            ,(V.KChar 'r', toggleRecents)
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
          editor <- E.handleEditorEvent ev (view urlInput s')
          continue $ set urlInput editor s'
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
          { _currentMethod = GET
          , _mode = Normal
          , _requestHistory = emptyZipper
          , _urlInput = input "urlInput" "http://"
          , _response = ""
          , _showRecents = False
          }
