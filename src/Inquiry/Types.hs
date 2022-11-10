{-# LANGUAGE TemplateHaskell #-}

module Inquiry.Types
  ( AppState (..),
    EditMode (..),
    FocusElement (..),
    currentMethod,
    requestHistory,
    urlInput,
    mode,
    showRecents,
    defaultUrl,
    focusedElement,
  )
where

import qualified Brick.Widgets.Edit as E
import Data.Text (Text)
import Inquiry.Request (Method, RequestHistory)
import Lens.Micro.Platform (makeLenses)

data EditMode = Ex | Normal | Insert deriving (Eq, Show)
data FocusElement = UrlField | HeadersField | ResponseField deriving (Bounded, Enum, Eq, Show)

data AppState = AppState
  { _currentMethod :: Method,
    _requestHistory :: RequestHistory,
    _urlInput :: E.Editor Text Text,
    _defaultUrl :: Text,
    _mode :: EditMode,
    _showRecents :: Bool,
    _focusedElement :: FocusElement
  }
  deriving (Show)

makeLenses ''AppState
