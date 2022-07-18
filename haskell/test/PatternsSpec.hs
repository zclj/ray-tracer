module PatternsSpec where

import System.IO.Unsafe (unsafePerformIO)

import Test.Tasty
import Test.Tasty.Hspec as HS

import Patterns as SUT
import Tuples

patternTests :: TestTree
patternTests = testGroup "Pattern Tests" [
  testGroup "Specs for"
  [ unsafePerformIO (testSpec "Patterns" patternBasics)]]

white = Color (Red 1) (Green 1) (Blue 1)
black = Color (Red 0) (Green 0) (Blue 0)

patternBasics :: Spec
patternBasics =
  describe "Patterns" $ do
    {- Scenario: Creating a stripe pattern
         Given pattern ← stripe_pattern(white, black)
         Then pattern.a = white
           And pattern.b = black -}
    describe "Creating a stripe pattern" $ do
      let p = SUT.stripePattern white black
      it "stripe pattern a is white" $ do
        a p `shouldBe` white
      it "stripe pattern b is black" $ do
        b p `shouldBe` black
    {- Scenario: A stripe pattern is constant in y
         Given pattern ← stripe_pattern(white, black)
         Then stripe_at(pattern, point(0, 0, 0)) = white
           And stripe_at(pattern, point(0, 1, 0)) = white
           And stripe_at(pattern, point(0, 2, 0)) = white -}
    describe "A stripe pattern is constant in y" $ do
      let p = SUT.stripePattern white black
      it "stripe_at(pattern, point(0, 0, 0)) = white" $ do
        stripeAt p (point 0 0 0) `shouldBe` white
      it "stripe_at(pattern, point(0, 1, 0)) = white" $ do
        stripeAt p (point 0 1 0) `shouldBe` white
      it "stripe_at(pattern, point(0, 2, 0)) = white" $ do
        stripeAt p (point 0 2 0) `shouldBe` white
    {- Scenario: A stripe pattern is constant in z
         Given pattern ← stripe_pattern(white, black)
         Then stripe_at(pattern, point(0, 0, 0)) = white
           And stripe_at(pattern, point(0, 0, 1)) = white
           And stripe_at(pattern, point(0, 0, 2)) = white -}
