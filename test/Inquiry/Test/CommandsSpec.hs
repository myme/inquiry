{-# LANGUAGE OverloadedStrings #-}

module Inquiry.Test.CommandsSpec where

import Inquiry.Commands
import Inquiry.Input
import Inquiry.Request
import Inquiry.Types
import qualified Inquiry.Zipper as Z
import Lens.Micro.Platform (view, (%~), (&), (.~), (^.))
import Test.Hspec

initialState :: AppState
initialState =
  AppState
    { _currentMethod = GET,
      _mode = Normal,
      _requestHistory = Z.emptyZipper,
      _showRecents = False,
      _urlInput = input "urlInput" "http://"
    }

spec :: SpecWith ()
spec = describe "Inquiry.Commands" $ do
  describe "history navigation" $ do
    it "empty history is emptyZipper" $ do
      let state = prevHistoryItem' initialState
      view requestHistory state `shouldBe` Z.emptyZipper

    it "prev of singleton list is Nothing" $ do
      let entry = (Request GET "http://example.com", Nothing)
          state = prevHistoryItem' $ initialState & requestHistory %~ Z.insert entry
      view requestHistory state `shouldBe` Z.Zipper Nothing [] [entry]

    it "next of singleton list is Nothing" $ do
      let entry = (Request GET "http://example.com", Nothing)
          s1 = nextHistoryItem' $ initialState & requestHistory %~ Z.insert entry
      view requestHistory s1 `shouldBe` Z.Zipper Nothing [entry] []

    it "updates currentMethod" $ do
      let entry = (Request POST "http://example.com", Nothing)
          state = initialState & requestHistory %~ Z.insert entry
          prev = prevHistoryItem' $ nextHistoryItem' state
          next = nextHistoryItem' $ prevHistoryItem' state
      view currentMethod state `shouldBe` GET
      view currentMethod prev `shouldBe` POST
      view currentMethod next `shouldBe` POST

    it "over zipper" $ do
      let reqs =
            [ (Request GET "http://foo.com", Nothing),
              (Request GET "http://bar.com", Nothing),
              (Request GET "http://baz.com", Nothing)
            ]
          s0 = initialState & requestHistory .~ foldr Z.insert Z.emptyZipper (reverse reqs)

      let s1 = nextHistoryItem' s0
      s1 ^. requestHistory `shouldBe` Z.Zipper Nothing (reverse reqs) []

      nextHistoryItem' s1 ^. requestHistory `shouldBe` s1 ^. requestHistory

      let s2 = prevHistoryItem' s0
      s2 ^. requestHistory `shouldBe` Z.Zipper (Just $ reqs !! 1) [head reqs] [reqs !! 2]

      prevHistoryItem' s2 ^. requestHistory `shouldBe` Z.Zipper (Just $ head reqs) [] (tail reqs)
