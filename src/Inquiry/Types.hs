{-# LANGUAGE TemplateHaskell #-}

module Inquiry.Types
  ( AppState(..)
  , EditMode(..)
  , Method(..)
  , Request(..)
  , method
  , url
  , currentRequest
  , requestHistory
  , urlInput
  , mode
  ) where

import qualified Brick.Widgets.Edit as E
import           Data.Text (Text, unpack)
import           Inquiry.Zipper (Zipper)
import           Lens.Micro.Platform (makeLenses)

data EditMode = Ex | Normal | Insert deriving (Eq, Show)

data Method = GET | POST deriving (Show, Eq)

data Request = Request { _method :: Method
                       , _url :: Text
                       } deriving (Eq)

instance Show Request where
  show (Request m u) = show m <> " " <> unpack u

data AppState = AppState { _currentRequest :: Request
                         , _requestHistory :: Zipper Request
                         , _urlInput :: E.Editor Text Text
                         , _mode :: EditMode
                         } deriving (Show)

makeLenses ''AppState
makeLenses ''Request
