{-# LANGUAGE TemplateHaskell #-}

module Inquiry.Request
  ( Method(..)
  , Request(..)
  , RequestHistory
  , Url
  , nextMethod
  , reqMethod
  , reqUrl
  ) where

import Data.Text (Text, unpack)
import Lens.Micro.Platform (makeLenses)

data Method = GET | POST deriving (Bounded, Enum, Eq, Show)
type Url = Text

data Request = Request { _reqMethod :: Method
                       , _reqUrl :: Url
                       } deriving (Eq, Ord)

instance Show Request where
  show req = show (_reqMethod req) <> " " <> unpack (_reqUrl req)

nextMethod :: Method -> Method
nextMethod m = cycle (enumFrom minBound) !! (fromEnum m + 1)

makeLenses ''Request
