module Inquiry.Test.Zipper where

import Data.Foldable (toList)
import Inquiry.Zipper
import Test.Hspec
import Test.QuickCheck

zipperTests :: SpecWith ()
zipperTests = describe "Inquiry.Zipper" $ do
  describe "tests" $ do
    it "emptyZipper is empty" $
      peek emptyZipper `shouldBe` (Nothing :: Maybe Int)

    it "can insert one element" $ do
      let z = insert "foo" emptyZipper
      peek z `shouldBe` Just "foo"

    it "can navigate off the front" $ do
      let z = prev $ insert "foo" emptyZipper
      peek z `shouldBe` Nothing
      prev z `shouldBe` z

    it "can navigate off the back" $ do
      let z = next $ insert "foo" emptyZipper
      peek z `shouldBe` Nothing
      next z `shouldBe` z

    it "manually inserting elements and get back list" $ do
      let z1 = insert "foo" emptyZipper
          z2 = insert "bar" z1
          z3 = insert "baz" z2
          z4 = insert "qux" z3
      toList z4 `shouldBe` ["foo", "bar", "baz", "qux"]

    it "can insert and navigate" $ do
      let z1 = insert "foo" emptyZipper
          z2 = insert "bar" z1
          z3 = prev z2
          z4 = next z3
      peek z3 `shouldBe` Just "foo"
      prev z3 `shouldBe` Zipper Nothing [] ["foo", "bar"]
      peek z4 `shouldBe` Just "bar"
      next z4 `shouldBe` Zipper Nothing ["bar", "foo"] []

    it "can navigate forward from start" $ do
      let z1 = start $ foldr insert emptyZipper ["baz", "bar", "foo"]
          z2 = next z1
      z1 `shouldBe` Zipper Nothing [] ["foo", "bar", "baz"]
      z2 `shouldBe` Zipper (Just "foo") [] ["bar", "baz"]

    it "can navigate back from end" $ do
      let z1 = end $ foldr insert emptyZipper ["baz", "bar", "foo"]
          z2 = prev z1
      z1 `shouldBe` Zipper Nothing ["baz", "bar", "foo"] []
      z2 `shouldBe` Zipper (Just "baz") ["bar", "foo"] []

    it "can pop items" $ do
      let z1 = insert "foo" emptyZipper
          z2 = insert "bar" z1
          z3 = insert "baz" z2
      pop emptyZipper `shouldBe` (Nothing, emptyZipper :: Zipper String)
      pop z3 `shouldBe` (Just "baz", z2)
      pop (prev z3) `shouldBe` (Just "bar", insert "baz" z1)

  describe "append" $ do
    it "can append while keeping cursor" $ do
      let zipper = prev $ foldr insert emptyZipper ["bar", "foo"]
      peek zipper `shouldBe` Just "foo"
      append "baz" zipper `shouldBe` Zipper (Just "foo") [] ["bar", "baz"]

  describe "start" $ do
    it "moves cursor to the start" $ do
      let z1 = insert "foo" emptyZipper
          z2 = insert "bar" z1
          z3 = insert "baz" z2
      start z3 `shouldBe` Zipper Nothing [] ["foo", "bar", "baz"]

  describe "end" $ do
    it "moves cursor to the end" $ do
      let z1 = insert "foo" emptyZipper
          z2 = insert "bar" z1
          z3 = insert "baz" z2
      end (start z3) `shouldBe` Zipper Nothing ["baz", "bar", "foo"] []

  describe "Foldable" $ do
    it "can create list with foldr" $ do
      let list = ["foo", "bar", "baz"]
          zipper = foldr insert emptyZipper $ reverse list
      toList zipper `shouldBe` list

    it "can ignores Nothing cursor" $ do
      let list = ["foo", "bar"]
          zipper = next $ foldr insert emptyZipper $ reverse list
      peek zipper `shouldBe` Nothing
      toList zipper `shouldBe` list

  describe "properties" $ do
    it "peek == inserted" $ property $ \x -> do
      let insertAndPeek :: String -> Maybe String
          insertAndPeek x' = peek $ insert x' emptyZipper
      insertAndPeek x == Just x

    it "folds to insert order" $ property $ \x -> do
      let zipper = foldr insert emptyZipper (reverse x :: [String])
      foldr (<>) mempty zipper == foldr (<>) mempty x
