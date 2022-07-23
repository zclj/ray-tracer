module PatternsSpec where

import System.IO.Unsafe (unsafePerformIO)

import Test.Tasty
import Test.Tasty.Hspec as HS

import Patterns as SUT
import Tuples
import Spheres
import Shapes
import Transformations

patternTests :: TestTree
patternTests = testGroup "Pattern Tests" [
  testGroup "Specs for"
  [ unsafePerformIO (testSpec "Patterns" patternBasics)
  , unsafePerformIO (testSpec "Patterns" patternTransformations)]]

white = Color (Red 1) (Green 1) (Blue 1)
black = Color (Red 0) (Green 0) (Blue 0)

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
      let s  = makeUnitSphere 1
          s' = s { sphereTransform = (scaling 2 2 2) }
          p  = SUT.stripePattern white black
          c  = stripeAtObject p s' (point 1.5 0 0)
      it "color at point is white" $ do
        c `shouldBe` white
    {- Scenario: Stripes with a pattern transformation
         Given object ← sphere()
           And pattern ← stripe_pattern(white, black)
           And set_pattern_transform(pattern, scaling(2, 2, 2))
         When c ← stripe_at_object(pattern, object, point(1.5, 0, 0))
         Then c = white -}
    describe "Stripes with a pattern transformation" $ do
      let s  = makeUnitSphere 1
          p  = SUT.stripePattern white black
          p' = p { patternTransform = scaling 2 2 2 }
          c  = stripeAtObject p' s (point 1.5 0 0)
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
      let s  = makeUnitSphere 1
          s' = s { sphereTransform = (scaling 2 2 2) }
          p  = SUT.stripePattern white black
          p' = p { patternTransform = translation 0.5 2 2 }
          c  = stripeAtObject p' s' (point 2.5 0 0)
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
