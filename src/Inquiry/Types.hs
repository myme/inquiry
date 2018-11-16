{-# LANGUAGE TemplateHaskell #-}

module Inquiry.Types
  ( AppState(..)
  , EditMode(..)
  , currentMethod
  , requestHistory
  , urlInput
  , mode
  , response
  , showRecents
  ) where

import qualified Brick.Widgets.Edit as E
import           Data.Text (Text)
import           Inquiry.Request (Method, Request)
import           Inquiry.Zipper (Zipper)
import           Lens.Micro.Platform (makeLenses)

data EditMode = Ex | Normal | Insert deriving (Eq, Show)

data AppState = AppState { _currentMethod :: Method
                         , _requestHistory :: Zipper Request
                         , _urlInput :: E.Editor Text Text
                         , _mode :: EditMode
                         , _response :: Text
                         , _showRecents :: Bool
                         } deriving (Show)

makeLenses ''AppState
