module TransformationsSpec where

import System.IO.Unsafe (unsafePerformIO)

import Test.Tasty
import Test.Tasty.Hspec as HS
import Test.Tasty.QuickCheck as QC

import Transformations as SUT
import Tuples

transformationTests :: TestTree
transformationTests = testGroup "Transformation Tests" [
  testGroup "Specs for"
  [ unsafePerformIO (testSpec "Transformation" transformationBasics)]]

transformationBasics :: Spec
transformationBasics =
  describe "Basics" $ do
    {- Scenario: Multiplying by a translation matrix
         Given transform ← translation(5, -3, 2)
           And p ← point(-3, 4, 5)
         Then transform * p = point(2, 1, 7) -}
    describe "Multiplying by a translation matrix" $ do
      let t = SUT.translation 5 (-3) 2
          p = point (-3) 4 5
      it "transform * point(-3, 4, 5) = point(2, 1, 7)" $ do
        SUT.transform p `shouldBe` point 2 1 7
