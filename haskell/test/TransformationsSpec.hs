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
    unsafePerformIO (testSpec "Transformation" transformationScaling),
    unsafePerformIO (testSpec "Transformation" transformationRotation),
    unsafePerformIO (testSpec "Transformation" transformationShearing)]]

transformationShearing :: Spec
transformationShearing =
  describe "Shearing" $ do
    {- Scenario: A shearing transformation moves x in proportion to y
         Given transform ← shearing(1, 0, 0, 0, 0, 0)
           And p ← point(2, 3, 4)
         Then transform * p = point(5, 3, 4) -}
    describe "A shearing transformation moves x in proportion to y" $ do
      let t = SUT.shearing 1 0 0 0 0 0
          p = point 2 3 4
      it "transform * p = point(5, 3, 4)" $ do
        mulT t p `shouldBe` point 5 3 4
    {- Scenario: A shearing transformation moves x in proportion to z
         Given transform ← shearing(0, 1, 0, 0, 0, 0)
           And p ← point(2, 3, 4)
         Then transform * p = point(6, 3, 4) -}
    describe "A shearing transformation moves x in proportion to z" $ do
      let t = SUT.shearing 0 1 0 0 0 0
          p = point 2 3 4
      it "transform * p = point(6, 3, 4)" $ do
        mulT t p `shouldBe` point 6 3 4
    {- Scenario: A shearing transformation moves y in proportion to x
         Given transform ← shearing(0, 0, 1, 0, 0, 0)
           And p ← point(2, 3, 4)
         Then transform * p = point(2, 5, 4) -}
    describe "A shearing transformation moves y in proportion to x" $ do
      let t = SUT.shearing 0 0 1 0 0 0
          p = point 2 3 4
      it "transform * p = point(2, 5, 4)" $ do
        mulT t p `shouldBe` point 2 5 4
    {- Scenario: A shearing transformation moves y in proportion to z
         Given transform ← shearing(0, 0, 0, 1, 0, 0)
           And p ← point(2, 3, 4)
         Then transform * p = point(2, 7, 4) -}
    describe "A shearing transformation moves y in proportion to z" $ do
      let t = SUT.shearing 0 0 0 1 0 0
          p = point 2 3 4
      it "transform * p = point(2, 7, 4)" $ do
        mulT t p `shouldBe` point 2 7 4
    {- Scenario: A shearing transformation moves z in proportion to x
         Given transform ← shearing(0, 0, 0, 0, 1, 0)
           And p ← point(2, 3, 4)
         Then transform * p = point(2, 3, 6) -}
    describe "A shearing transformation moves z in proportion to x" $ do
      let t = SUT.shearing 0 0 0 0 1 0
          p = point 2 3 4
      it "transform * p = point(2, 3, 6)" $ do
        mulT t p `shouldBe` point 2 3 6
    {- Scenario: A shearing transformation moves z in proportion to y
         Given transform ← shearing(0, 0, 0, 0, 0, 1)
           And p ← point(2, 3, 4)
         Then transform * p = point(2, 3, 7) -}
    describe "A shearing transformation moves z in proportion to y" $ do
      let t = SUT.shearing 0 0 0 0 0 1
          p = point 2 3 4
      it "transform * p = point(2, 3, 7)" $ do
        mulT t p `shouldBe` point 2 3 7

transformationRotation :: Spec
transformationRotation =
  describe "Rotation" $ do
    {- Scenario: Rotating a point around the x axis
         Given p ← point(0, 1, 0)
           And half_quarter ← rotation_x(π / 4)
           And full_quarter ← rotation_x(π / 2)
         Then half_quarter * p = point(0, √2/2, √2/2)
           And full_quarter * p = point(0, 0, 1) -}
    describe "Rotating a point around the x axis" $ do
      let p  = point 0 1 0
          hq = SUT.rotationX (pi/4)
          fq = SUT.rotationX (pi/2)
      it "half_quarter * p = point(0, √2/2, √2/2)" $ do
        mulT hq p `shouldBe` point 0 (sqrt 2 / 2) (sqrt 2 / 2)
      it "full_quarter * p = point(0, 0, 1)" $ do
        mulT fq p `shouldBe` point 0 0 1
    {- Scenario: The inverse of an x-rotation rotates in the opposite direction
         Given p ← point(0, 1, 0)
           And half_quarter ← rotation_x(π / 4)
           And inv ← inverse(half_quarter)
         Then inv * p = point(0, √2/2, -√2/2) -}
    describe "The inverse of an x-rotation rotates in the opposite direction" $ do
      let p  = point 0 1 0
          hq = SUT.rotationX (pi/4)
          i  = inverse hq
      it "inv * p = point(0, √2/2, -√2/2)" $ do
        mulT i p `shouldBe` point 0 (sqrt 2 / 2) (-(sqrt 2 / 2))
    {- Scenario: Rotating a point around the y axis
         Given p ← point(0, 0, 1)
           And half_quarter ← rotation_y(π / 4)
           And full_quarter ← rotation_y(π / 2)
         Then half_quarter * p = point(√2/2, 0, √2/2)
           And full_quarter * p = point(1, 0, 0) -}
    describe "Rotating a point around the y axis" $ do
      let p  = point 0 0 1
          hq = SUT.rotationY (pi/4)
          fq = SUT.rotationY (pi/2)
      it "half_quarter * p = point(√2/2, 0, √2/2)" $ do
        mulT hq p `shouldBe` point (sqrt 2 / 2) 0 (sqrt 2 / 2)
      it "full_quarter * p = point(1, 0, 0)" $ do
        mulT fq p `shouldBe` point 1 0 0
    {- Scenario: Rotating a point around the z axis
         Given p ← point(0, 1, 0)
           And half_quarter ← rotation_z(π / 4)
           And full_quarter ← rotation_z(π / 2)
         Then half_quarter * p = point(-√2/2, √2/2, 0)
           And full_quarter * p = point(-1, 0, 0) -}
    describe "Rotating a point around the z axis" $ do
      let p  = point 0 1 0
          hq = SUT.rotationZ (pi/4)
          fq = SUT.rotationZ (pi/2)
      it "half_quarter * p = point(-√2/2, √2/2, 0)" $ do
        mulT hq p `shouldBe` point (-(sqrt 2 / 2)) (sqrt 2 / 2) 0
      it "full_quarter * p = point(-1, 0, 0)" $ do
        mulT fq p `shouldBe` point (-1) 0 0

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
    {- Scenario: Multiplying by the inverse of a scaling matrix
         Given transform ← scaling(2, 3, 4)
           And inv ← inverse(transform)
           And v ← vector(-4, 6, 8)
         Then inv * v = vector(-2, 2, 2) -}
    describe "Multiplying by the inverse of a scaling matrix" $ do
      let t = SUT.scaling 2 3 4
          i = inverse t
          v = vector (-4) 6 8
      it "inv * v = vector(-2, 2, 2)" $ do
        mulT i v `shouldBe` vector (-2) 2 2
    {- Scenario: Reflection is scaling by a negative value
         Given transform ← scaling(-1, 1, 1)
           And p ← point(2, 3, 4)
         Then transform * p = point(-2, 3, 4) -}
    describe "Reflection is scaling by a negative value" $ do
      let t = SUT.scaling (-1) 1 1
          p = point 2 3 4
      it "transform * p = point(-2, 3, 4)" $ do
        mulT t p `shouldBe` point (-2) 3 4

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
