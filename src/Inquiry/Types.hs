{-# LANGUAGE TemplateHaskell #-}

module Inquiry.Types
  ( AppState (..),
    EditMode (..),
    currentMethod,
    requestHistory,
    urlInput,
    mode,
    showRecents,
    defaultState,
  )
where

import qualified Brick.Widgets.Edit as E
import Data.Text (Text)
import Inquiry.Input (input)
import Inquiry.Request (Method (GET), RequestHistory)
import Inquiry.Zipper (emptyZipper)
import Lens.Micro.Platform (makeLenses)

data EditMode = Ex | Normal | Insert deriving (Eq, Show)

data AppState = AppState
  { _currentMethod :: Method,
    _requestHistory :: RequestHistory,
    _urlInput :: E.Editor Text Text,
    _mode :: EditMode,
    _showRecents :: Bool
  }
  deriving (Show)

defaultState :: AppState
defaultState =
  AppState
    { _currentMethod = GET,
      _requestHistory = emptyZipper,
      _urlInput = input "urlInput" "https://example.com",
      _mode = Normal,
      _showRecents = False
    }

makeLenses ''AppState
