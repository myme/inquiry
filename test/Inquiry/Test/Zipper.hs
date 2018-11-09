module Inquiry.Test.Zipper where

import Test.Hspec
import Test.QuickCheck

import Inquiry.Zipper

zipperTests :: SpecWith ()
zipperTests = describe "Inquiry.Zipper" $ do
  describe "tests" $ do
    it "emptyZipper is empty" $
      peekZipper emptyZipper `shouldBe` (Nothing :: Maybe Int)

    it "can insert one element" $ do
      let z = insertZipper "foo" emptyZipper
      peekZipper z `shouldBe` Just "foo"

    it "can navigate off the front" $ do
      let z = prevZipper $ insertZipper "foo" emptyZipper
      peekZipper z `shouldBe` Nothing

    it "can navigate off the back" $ do
      let z = nextZipper $ insertZipper "foo" emptyZipper
      peekZipper z `shouldBe` Nothing

    it "manually inserting elements and get back list" $ do
      let z1 = insertZipper "foo" emptyZipper
          z2 = insertZipper "bar" z1
          z3 = insertZipper "baz" z2
          z4 = insertZipper "qux" z3
      foldr (:) [] z4 `shouldBe` ["foo", "bar", "baz", "qux"]

    it "can insert and navigate" $ do
      let z1 = insertZipper "foo" emptyZipper
          z2 = insertZipper "bar" z1
          z3 = prevZipper z2
          z4 = nextZipper z3
      peekZipper z3 `shouldBe` Just "foo"
      prevZipper z3 `shouldBe` Zipper Nothing [] ["foo", "bar"]
      peekZipper z4 `shouldBe` Just "bar"
      nextZipper z4 `shouldBe` Zipper Nothing ["bar", "foo"] []

    it "can pop items" $ do
      let z1 = insertZipper "foo" emptyZipper
          z2 = insertZipper "bar" z1
          z3 = insertZipper "baz" z2
      popZipper emptyZipper `shouldBe` (Nothing, emptyZipper :: Zipper String)
      popZipper z3 `shouldBe` (Just "baz", z2)
      popZipper (prevZipper z3) `shouldBe` (Just "bar", insertZipper "baz" z1)

  describe "append" $ do
    it "can append while keeping cursor" $ do
      let zipper = prevZipper $ foldr insertZipper emptyZipper ["bar", "foo"]
      peekZipper zipper `shouldBe` Just "foo"
      appendZipper "baz" zipper `shouldBe` Zipper (Just "foo") [] ["bar", "baz"]

  describe "gotoStart" $ do
    it "moves cursor to the start" $ do
      let z1 = insertZipper "foo" emptyZipper
          z2 = insertZipper "bar" z1
          z3 = insertZipper "baz" z2
      gotoStart z3 `shouldBe` Zipper Nothing [] ["foo", "bar", "baz"]

  describe "gotoEnd" $ do
    it "moves cursor to the end" $ do
      let z1 = insertZipper "foo" emptyZipper
          z2 = insertZipper "bar" z1
          z3 = insertZipper "baz" z2
      gotoEnd (gotoStart z3) `shouldBe` Zipper Nothing ["baz", "bar", "foo"] []

  describe "Foldable" $ do
    it "can create list with foldr" $ do
      let list = ["foo", "bar", "baz"]
          zipper = foldr insertZipper emptyZipper $ reverse list
      foldr (:) [] zipper `shouldBe` foldr (:) [] list

    it "can ignores Nothing cursor" $ do
      let list = ["foo", "bar"]
          zipper = nextZipper $ foldr insertZipper emptyZipper $ reverse list
      peekZipper zipper `shouldBe` Nothing
      foldr (:) [] zipper `shouldBe` foldr (:) [] list

  describe "properties" $ do
    it "peek == inserted" $ property $ \x -> do
      let insertAndPeek :: String -> Maybe String
          insertAndPeek x' = peekZipper $ insertZipper x' emptyZipper
      insertAndPeek x == Just x

    it "folds to insert order" $ property $ \x -> do
      let zipper = foldr insertZipper emptyZipper (reverse x :: [String])
      foldr (<>) mempty zipper == foldr (<>) mempty x

    -- it "navigating zipper does not change fold" $ property $
