{-# LANGUAGE TemplateHaskell #-}

module Inquiry.Types
  ( AppState(..)
  , EditMode(..)
  , Method(..)
  , Request(..)
  , method
  , url
  , currentRequest
  , recentReqs
  , urlInput
  , mode
  ) where

import qualified Brick.Widgets.Edit as E
import           Data.Text (Text, unpack)
import           Lens.Micro.Platform (makeLenses)

data EditMode = Ex | Normal | Insert deriving (Eq, Show)

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
