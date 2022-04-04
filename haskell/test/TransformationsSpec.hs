module TransformationsSpec where

import System.IO.Unsafe (unsafePerformIO)

import Test.Tasty
import Test.Tasty.Hspec as HS

import Transformations as SUT
import Matrices
import Tuples

transformationTests :: TestTree
transformationTests = testGroup "Transformation Tests" [
  testGroup "Specs for"
  [ unsafePerformIO (testSpec "Transformation" transformationTranslation),
    unsafePerformIO (testSpec "Transformation" transformationScaling)]]

transformationScaling :: Spec
transformationScaling =
  describe "Scaling" $ do
    {- Scenario: A scaling matrix applied to a point
         Given transform ← scaling(2, 3, 4)
           And p ← point(-4, 6, 8)
         Then transform * p = point(-8, 18, 32) -}
    describe "A scaling matrix applied to a point" $ do
      let t = SUT.scaling 2 3 4
          p = point (-4) 6 8
      it "transform * p = point(-8, 18, 32)" $ do
        mulT t p `shouldBe` point (-8) 18 32
    {- Scenario: A scaling matrix applied to a vector
         Given transform ← scaling(2, 3, 4)
           And v ← vector(-4, 6, 8)
         Then transform * v = vector(-8, 18, 32) -}
    describe "A scaling matrix applied to a vector" $ do
      let t = SUT.scaling 2 3 4
          v = vector (-4) 6 8
      it "transform * v = vector(-8, 18, 32)" $ do
        mulT t v `shouldBe` vector (-8) 18 32
transformationTranslation :: Spec
transformationTranslation =
  describe "Translation" $ do
    {- Scenario: Multiplying by a translation matrix
         Given transform ← translation(5, -3, 2)
           And p ← point(-3, 4, 5)
         Then transform * p = point(2, 1, 7) -}
    describe "Multiplying by a translation matrix" $ do
      let t = SUT.translation 5 (-3) 2
          p = point (-3) 4 5
      it "transform * point(-3, 4, 5) = point(2, 1, 7)" $ do
        mulT t p `shouldBe` point 2 1 7
    {- Scenario: Multiplying by the inverse of a translation matrix
         Given transform ← translation(5, -3, 2)
           And inv ← inverse(transform)
           And p ← point(-3, 4, 5)
         Then inv * p = point(-8, 7, 3) -}
    describe "Multiplying by the inverse of a translation matrix" $ do
      let t = SUT.translation 5 (-3) 2
          i = inverse t
          p = point (-3) 4 5
      it "inv * p = point(-8, 7, 3)" $ do
         mulT i p `shouldBe` point (-8) 7 3
    {- Scenario: Translation does not affect vectors
         Given transform ← translation(5, -3, 2)
           And v ← vector(-3, 4, 5)
         Then transform * v = v -}
    describe "Translation does not affect vectors" $ do
      let t = SUT.translation 5 (-3) 2
          v = vector (-3) 4 5
      it "transform * V = V" $ do
        mulT t v `shouldBe` v
