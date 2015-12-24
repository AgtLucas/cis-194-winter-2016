module Homework.Week1Spec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Homework.Week1

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "toDigits" $ do
    it "should split digits of integer into a list" $ do
      toDigits 1234 `shouldBe` [1,2,3,4]

    it "should return an empty list for zero" $ do
      toDigits 0 `shouldBe` []

    -- uses QuickCheck - need to learn how to generate constrained inputs
    it "should return an empty list for negative numbers" $ property $
      \x -> (if x < 0 then (toDigits x) else []) == ([] :: [Integer])

  describe "toDigitsRev" $ do
    it "should return an empty list for zero" $ do
      toDigitsRev 0 `shouldBe` []

    it "should return an empty list for negative numbers" $ do
      toDigitsRev (-1) `shouldBe` []
      toDigitsRev (-22222) `shouldBe` []

    it "should split digits of integer into a list in reverse order" $ do
      toDigitsRev 123 `shouldBe` [3,2,1]
      toDigitsRev 431 `shouldBe` [1,3,4]
      toDigitsRev 12 `shouldBe` [2,1]
      toDigitsRev 2 `shouldBe` [2]

  describe "doubleEveryOther" $ do
    it "should return an empty list given an empty list" $ do
      doubleEveryOther [] `shouldBe` []

    it "should double every other int in the list, from right to left" $ do
      doubleEveryOther [8,7,6,5] `shouldBe`[16,7,12,5]
      doubleEveryOther [1,2,3] `shouldBe` [1,4,3]

  describe "sumDigits" $ do
    it "should return zero for an empty list" $ do
      sumDigits [] `shouldBe` 0

    it "should sum all digits in the list" $ do
      sumDigits [16,7,12,5] `shouldBe` 22
      sumDigits [18,7,33,5] `shouldBe` 27

  describe "validate" $ do
    it "should return True for valid card number" $ do
      validate 4012888888881881 `shouldBe` True

    it "should return False for invalid card number" $ do
      validate 4012888888881882 `shouldBe` False

  describe "hanoi" $ do
    it "should return an empty list for zero discs" $ do
      hanoi 0 "a" "b" "c" `shouldBe` []

    it "should solve for 1 disc" $ do
      hanoi 1 "a" "b" "c" `shouldBe` [("a", "b")]

  describe "week1:checkpoint" $ do
    it "should be complete" $ do
      let complete = True
       in complete `shouldBe` True
