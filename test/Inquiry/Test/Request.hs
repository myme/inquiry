{-# LANGUAGE OverloadedStrings #-}

module Inquiry.Test.Request where

import qualified Data.Map.Strict as M
import           Data.Text (Text)
import           Inquiry.Request
import           Test.Hspec

requestTests :: SpecWith ()
requestTests = describe "Inquiry.Request" $ do
  describe "History" $ do
    it "can add request to history" $ do
      let req = Request GET "http://example.com"
          empty = M.empty :: RequestHistory Text
          history =  M.insert req "Hello, World!" empty
      snd (M.elemAt 0 history) `shouldBe` "Hello, World!"
