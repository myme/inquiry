{-# LANGUAGE TemplateHaskell #-}

module Inquiry.Request
  ( Method(..)
  , Request(..)
  , Response(..)
  , RequestHistory
  , Url
  , nextMethod
  , reqMethod
  , reqUrl
  , resBody
  , resCode
  ) where

import Data.Text (Text, unpack)
import Inquiry.Zipper (Zipper)
import Lens.Micro.Platform (makeLenses)

data Method = GET | POST deriving (Bounded, Enum, Eq, Show)

type Url = Text

data Request = Request { _reqMethod :: Method
                       , _reqUrl :: Url
                       } deriving (Eq)

instance Show Request where
  show req = show (_reqMethod req) <> " " <> unpack (_reqUrl req)

data Response = Response { _resCode :: Int
                         , _resBody :: Text
                         } deriving (Eq, Show)

type RequestHistory = Zipper (Request, Maybe Response)

nextMethod :: Method -> Method
nextMethod m = cycle (enumFrom minBound) !! (fromEnum m + 1)

makeLenses ''Request
makeLenses ''Response
