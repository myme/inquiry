{-# LANGUAGE TemplateHaskell #-}

module Inquiry.Request
  ( Method(..)
  , Request(..)
  , method
  , url
  , nextMethod
  ) where

import Data.Text (Text, unpack)
import Lens.Micro.Platform (makeLenses)

data Method = GET | POST deriving (Bounded, Enum, Eq, Show)

data Request = Request { _method :: Method
                       , _url :: Text
                       } deriving (Eq)

instance Show Request where
  show (Request m u) = show m <> " " <> unpack u

nextMethod :: Method -> Method
nextMethod m = cycle (enumFrom minBound) !! (fromEnum m + 1)

makeLenses ''Request
