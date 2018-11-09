{-# LANGUAGE OverloadedStrings #-}

module Inquiry.Test.Commands where

import           Inquiry.Commands
import           Inquiry.Input
import           Inquiry.Types
import qualified Inquiry.Zipper as Z
import           Lens.Micro.Platform ((^.), (.~), (%~), (&), view)
import           Test.Hspec

initialState :: AppState
initialState = AppState
               { _currentMethod = GET
               , _mode = Normal
               , _requestHistory = Z.emptyZipper
               , _urlInput = input "urlInput" "http://"
               }

commandsTests :: SpecWith ()
commandsTests = describe "Inquiry.Commands" $ do
  describe "history navigation" $ do
    it "empty history is emptyZipper" $ do
      let state = prevHistoryItem' initialState
      view requestHistory state `shouldBe` Z.emptyZipper

    it "prev of singleton list is Nothing" $ do
      let req = Request GET "http://example.com"
          state = prevHistoryItem' $ initialState & requestHistory %~ Z.insert req
      view requestHistory state `shouldBe` Z.Zipper Nothing [] [req]

    it "next of singleton list is Nothing" $ do
      let req = Request GET "http://example.com"
          s1 = nextHistoryItem' $ initialState & requestHistory %~ Z.insert req
      view requestHistory s1 `shouldBe` Z.Zipper Nothing [req] []

    it "over zipper" $ do
      let reqs = [ Request GET "http://foo.com"
                 , Request GET "http://bar.com"
                 , Request GET "http://baz.com"
                 ]
          s0 = initialState & requestHistory .~ foldr Z.insert Z.emptyZipper (reverse reqs)

      let s1 = nextHistoryItem' s0
      s1 ^. requestHistory `shouldBe` Z.Zipper Nothing (reverse reqs) []

      nextHistoryItem' s1 ^. requestHistory `shouldBe` s1 ^. requestHistory

      let s2 = prevHistoryItem' s0
      s2 ^. requestHistory `shouldBe` Z.Zipper (Just $ reqs !! 1) [head reqs] [reqs !! 2]

      prevHistoryItem' s2 ^. requestHistory `shouldBe` Z.Zipper (Just $ head reqs) [] (tail reqs)
