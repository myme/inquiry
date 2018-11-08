module Inquiry.Test.Zipper where

import Test.Hspec
import Test.QuickCheck

import Inquiry.Zipper

zipperTests :: SpecWith ()
zipperTests = describe "Inquiry.Zipper" $ do
  describe "tests" $ do
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
      prevZipper z3 `shouldBe` z3
      peekZipper z4 `shouldBe` Just "bar"
      nextZipper z4 `shouldBe` z4

    it "can pop items" $ do
      let z1 = insertZipper "foo" emptyZipper
          z2 = insertZipper "bar" z1
          z3 = insertZipper "baz" z2
      popZipper emptyZipper `shouldBe` (Nothing, emptyZipper :: Zipper String)
      popZipper z3 `shouldBe` (Just "baz", z2)
      popZipper (prevZipper z3) `shouldBe` (Just "bar", insertZipper "baz" z1)

    it "can create list with foldr" $ do
      let list = [1 .. 10] :: [Int]
          zipper = foldr insertZipper emptyZipper $ reverse list
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
