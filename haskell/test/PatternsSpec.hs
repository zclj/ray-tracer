module PatternsSpec where

import System.IO.Unsafe (unsafePerformIO)

import Test.Tasty
import Test.Tasty.Hspec as HS

import Patterns as SUT
import Tuples
import Shapes
import Transformations

patternTests :: TestTree
patternTests = testGroup "Pattern Tests" [
  testGroup "Specs for"
  [ unsafePerformIO (testSpec "Patterns" patternBasics)
  , unsafePerformIO (testSpec "Patterns" patternTransformations)
  , unsafePerformIO (testSpec "Patterns" patterns)]]

white = Color (Red 1) (Green 1) (Blue 1)
black = Color (Red 0) (Green 0) (Blue 0)

patterns :: Spec
patterns =
  describe "Patterns" $ do
    {- Scenario: A gradient linearly interpolates between colors
         Given pattern ← gradient_pattern(white, black)
         Then pattern_at(pattern, point(0, 0, 0)) = white
           And pattern_at(pattern, point(0.25, 0, 0)) = color(0.75, 0.75, 0.75)
           And pattern_at(pattern, point(0.5, 0, 0)) = color(0.5, 0.5, 0.5)
           And pattern_at(pattern, point(0.75, 0, 0)) = color(0.25, 0.25, 0.25) -}
    describe "A gradient linearly interpolates between colors" $ do
      let p  = SUT.gradientPattern white black
          c1 = SUT.patternAt p (point 0 0 0)
          c2 = SUT.patternAt p (point 0.25 0 0)
          c3 = SUT.patternAt p (point 0.5 0 0)
          c4 = SUT.patternAt p (point 0.75 0 0)
      it "color at (0, 0, 0) = white" $ do
        c1 `shouldBe` white
      it "color at (0.25, 0, 0) = color(0.75, 0.75, 0.75)" $ do
        c2 `shouldBe` Color (Red 0.75) (Green 0.75) (Blue 0.75)
      it "color at (0.5, 0, 0) = color(0.5, 0.5, 0.5)" $ do
        c3 `shouldBe` Color (Red 0.5) (Green 0.5) (Blue 0.5)
      it "color at (0.75, 0, 0) = color(0.25, 0.25, 0.25)" $ do
        c4 `shouldBe` Color (Red 0.25) (Green 0.25) (Blue 0.25)
    {- Scenario: A ring should extend in both x and z
         Given pattern ← ring_pattern(white, black)
         Then pattern_at(pattern, point(0, 0, 0)) = white
           And pattern_at(pattern, point(1, 0, 0)) = black
           And pattern_at(pattern, point(0, 0, 1)) = black
           # 0.708 = just slightly more than √2/2
           And pattern_at(pattern, point(0.708, 0, 0.708)) = black -}
    describe "A ring should extend in both x and z" $ do
      let p  = SUT.ringPattern white black
          c1 = SUT.patternAt p (point 0 0 0)
          c2 = SUT.patternAt p (point 1 0 0)
          c3 = SUT.patternAt p (point 0 0 1)
          c4 = SUT.patternAt p (point 0.708 0 0.708)
      it "color at (0, 0, 0) = white" $ do
        c1 `shouldBe` white
      it "color at (1, 0, 0) = black" $ do
        c2 `shouldBe` black
      it "color at (0, 0, 1) = black" $ do
        c3 `shouldBe` black
      it "color at (0.708, 0, 0.708) = black" $ do
        c4 `shouldBe` black
    {- Scenario: Checkers should repeat in x
         Given pattern ← checkers_pattern(white, black)
         Then pattern_at(pattern, point(0, 0, 0)) = white
           And pattern_at(pattern, point(0.99, 0, 0)) = white
           And pattern_at(pattern, point(1.01, 0, 0)) = black -}
    describe "Checkers should repeat in x" $ do
      let p  = SUT.checkersPattern white black
          c1 = SUT.patternAt p (point 0 0 0)
          c2 = SUT.patternAt p (point 0.99 0 0)
          c3 = SUT.patternAt p (point 1.01 0 0)
      it "color at (0, 0, 0) = white" $ do
        c1 `shouldBe` white
      it "color at (0.99, 0, 0) = white" $ do
        c2 `shouldBe` white
      it "color at (1.01, 0, 0) = black" $ do
        c3 `shouldBe` black
    {- Scenario: Checkers should repeat in y
         Given pattern ← checkers_pattern(white, black)
         Then pattern_at(pattern, point(0, 0, 0)) = white
           And pattern_at(pattern, point(0, 0.99, 0)) = white
           And pattern_at(pattern, point(0, 1.01, 0)) = black -}
    describe "Checkers should repeat in y" $ do
      let p  = SUT.checkersPattern white black
          c1 = SUT.patternAt p (point 0 0 0)
          c2 = SUT.patternAt p (point 0 0.99 0)
          c3 = SUT.patternAt p (point 0 1.01 0)
      it "color at (0, 0, 0) = white" $ do
        c1 `shouldBe` white
      it "color at (0, 0.99, 0) = white" $ do
        c2 `shouldBe` white
      it "color at (0, 1.01, 0) = black" $ do
        c3 `shouldBe` black
    {- Scenario: Checkers should repeat in z
         Given pattern ← checkers_pattern(white, black)
         Then pattern_at(pattern, point(0, 0, 0)) = white
           And pattern_at(pattern, point(0, 0, 0.99)) = white
           And pattern_at(pattern, point(0, 0, 1.01)) = black -}
    describe "Checkers should repeat in z" $ do
      let p  = SUT.checkersPattern white black
          c1 = SUT.patternAt p (point 0 0 0)
          c2 = SUT.patternAt p (point 0 0 0.99)
          c3 = SUT.patternAt p (point 0 0 1.01)
      it "color at (0, 0, 0) = white" $ do
        c1 `shouldBe` white
      it "color at (0, 0, 0.99) = white" $ do
        c2 `shouldBe` white
      it "color at (0, 0, 1.01) = black" $ do
        c3 `shouldBe` black

patternTransformations :: Spec
patternTransformations =
  describe "Patterns" $ do
    {- Scenario: Stripes with an object transformation
         Given object ← sphere()
           And set_transform(object, scaling(2, 2, 2))
           And pattern ← stripe_pattern(white, black)
         When c ← stripe_at_object(pattern, object, point(1.5, 0, 0))
         Then c = white -}
    describe "Stripes with an object transformation" $ do
      let s  = defaultSphere 1
          s' = s { ashapeTransform = scaling 2 2 2 }
          p  = SUT.stripePattern white black
          c  = patternAtShape p s' (point 1.5 0 0)
      it "color at point is white" $ do
        c `shouldBe` white
    {- Scenario: Stripes with a pattern transformation
         Given object ← sphere()
           And pattern ← stripe_pattern(white, black)
           And set_pattern_transform(pattern, scaling(2, 2, 2))
         When c ← stripe_at_object(pattern, object, point(1.5, 0, 0))
         Then c = white -}
    describe "Stripes with a pattern transformation" $ do
      let s  = defaultSphere 1
          p  = SUT.stripePattern white black
          p' = p { patternTransform = scaling 2 2 2 }
          c  = patternAtShape p' s (point 1.5 0 0)
      it "color at point is white" $ do
        c `shouldBe` white
    {- Scenario: Stripes with both an object and a pattern transformation
         Given object ← sphere()
           And set_transform(object, scaling(2, 2, 2))
           And pattern ← stripe_pattern(white, black)
           And set_pattern_transform(pattern, translation(0.5, 0, 0))
         When c ← stripe_at_object(pattern, object, point(2.5, 0, 0))
         Then c = white -}
    describe "Stripes with both an object and a pattern transformation" $ do
      let s  = defaultSphere 1
          s' = s { ashapeTransform = scaling 2 2 2 }
          p  = SUT.stripePattern white black
          p' = p { patternTransform = translation 0.5 2 2 }
          c  = patternAtShape p' s' (point 2.5 0 0)
      it "color at point is white" $ do
        c `shouldBe` white

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
    describe "A stripe pattern is constant in z" $ do
      let p = SUT.stripePattern white black
      it "stripe_at(pattern, point(0, 0, 0)) = white" $ do
        stripeAt p (point 0 0 0) `shouldBe` white
      it "stripe_at(pattern, point(0, 0, 1)) = white" $ do
        stripeAt p (point 0 0 1) `shouldBe` white
      it "stripe_at(pattern, point(0, 0, 2)) = white" $ do
        stripeAt p (point 0 0 2) `shouldBe` white
    {- Scenario: A stripe pattern alternates in x
         Given pattern ← stripe_pattern(white, black)
         Then stripe_at(pattern, point(0, 0, 0)) = white
           And stripe_at(pattern, point(0.9, 0, 0)) = white
           And stripe_at(pattern, point(1, 0, 0)) = black
           And stripe_at(pattern, point(-0.1, 0, 0)) = black
           And stripe_at(pattern, point(-1, 0, 0)) = black
           And stripe_at(pattern, point(-1.1, 0, 0)) = white -}
    describe "A stripe pattern alternates in x" $ do
      let p = SUT.stripePattern white black
      it "stripe_at(pattern, point(0, 0, 0)) = white" $ do
        stripeAt p (point 0 0 0) `shouldBe` white
      it "stripe_at(pattern, point(0.9, 0, 0)) = white" $ do
        stripeAt p (point 0.9 0 0) `shouldBe` white
      it "stripe_at(pattern, point(1, 0, 0)) = black" $ do
        stripeAt p (point 1 0 0) `shouldBe` black
      it "stripe_at(pattern, point(-0.1, 0, 0)) = black" $ do
        stripeAt p (point (-0.1) 0 0) `shouldBe` black
      it "stripe_at(pattern, point(-1, 0, 0)) = black" $ do
        stripeAt p (point (-1) 0 0) `shouldBe` black
      it "stripe_at(pattern, point(-1.1, 0, 0)) = white" $ do
        stripeAt p (point (-1.1) 0 0) `shouldBe` white
