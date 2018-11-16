{-# LANGUAGE TemplateHaskell #-}

module Inquiry.Types
  ( AppState(..)
  , EditMode(..)
  , Method(..)
  , Request(..)
  , method
  , url
  , currentMethod
  , nextMethod
  , requestHistory
  , urlInput
  , mode
  , response
  , showRecents
  ) where

import qualified Brick.Widgets.Edit as E
import           Data.Text (Text, unpack)
import           Inquiry.Zipper (Zipper)
import           Lens.Micro.Platform (makeLenses)

data EditMode = Ex | Normal | Insert deriving (Eq, Show)

data Method = GET | POST deriving (Bounded, Enum, Eq, Show)

nextMethod :: Method -> Method
nextMethod m = cycle (enumFrom minBound) !! (fromEnum m + 1)

data Request = Request { _method :: Method
                       , _url :: Text
                       } deriving (Eq)

instance Show Request where
  show (Request m u) = show m <> " " <> unpack u

data AppState = AppState { _currentMethod :: Method
                         , _requestHistory :: Zipper Request
                         , _urlInput :: E.Editor Text Text
                         , _mode :: EditMode
                         , _response :: Text
                         , _showRecents :: Bool
                         } deriving (Show)

makeLenses ''AppState
makeLenses ''Request
