module TransformationsSpec where

import System.IO.Unsafe (unsafePerformIO)

import Test.Tasty
import Test.Tasty.Hspec as HS

import Transformations as SUT
import Matrices as M
import Tuples

transformationTests :: TestTree
transformationTests = testGroup "Transformation Tests" [
  testGroup "Specs for"
  [ unsafePerformIO (testSpec "Transformation" transformationTranslation),
    unsafePerformIO (testSpec "Transformation" transformationScaling),
    unsafePerformIO (testSpec "Transformation" transformationRotation),
    unsafePerformIO (testSpec "Transformation" transformationShearing),
    unsafePerformIO (testSpec "Transformation" transformationApplication)
  , unsafePerformIO (testSpec "Transformation" transformationView)]]

transformationView :: Spec
transformationView =
  describe "View" $ do
    {- Scenario: The transformation matrix for the default orientation
         Given from ← point(0, 0, 0)
           And to ← point(0, 0, -1)
           And up ← vector(0, 1, 0)
         When t ← view_transform(from, to, up)
         Then t = identity_matrix -}
    describe "The transformation matrix for the default orientation" $ do
      let from = point 0 0 0
          to   = point 0 0 (-1)
          up   = vector 0 1 0
          t    = SUT.viewTransform from to up
      it "t = identity_matrix" $ do
        t `shouldBe` identityV
    {- Scenario: A view transformation matrix looking in positive z direction
         Given from ← point(0, 0, 0)
           And to ← point(0, 0, 1)
           And up ← vector(0, 1, 0)
         When t ← view_transform(from, to, up)
         Then t = scaling(-1, 1, -1) -}
    describe "A view transformation matrix looking in positive z direction" $ do
      let from = point 0 0 0
          to   = point 0 0 1
          up   = vector 0 1 0
          t    = SUT.viewTransform from to up
      it "t = scaling(-1, 1, -1)" $ do
        t `shouldBe` SUT.scaling (-1) 1 (-1)
    {- Scenario: The view transformation moves the world
         Given from ← point(0, 0, 8)
           And to ← point(0, 0, 0)
           And up ← vector(0, 1, 0)
         When t ← view_transform(from, to, up)
         Then t = translation(0, 0, -8) -}
    describe "The view transformation moves the world" $ do
      let from = point 0 0 8
          to   = point 0 0 0
          up   = vector 0 1 0
          t    = SUT.viewTransform from to up
      it "t = translation(0, 0, -8)" $ do
        t `shouldBe` SUT.translation 0 0 (-8)
    {- Scenario: An arbitrary view transformation
         Given from ← point(1, 3, 2)
           And to ← point(4, -2, 8)
           And up ← vector(1, 1, 0)
         When t ← view_transform(from, to, up)
         Then t is the following 4x4 matrix:
             | -0.50709 | 0.50709 |  0.67612 | -2.36643 |
             |  0.76772 | 0.60609 |  0.12122 | -2.82843 |
             | -0.35857 | 0.59761 | -0.71714 |  0.00000 |
             |  0.00000 | 0.00000 |  0.00000 |  1.00000 | -}
    describe "An arbitrary view transformation" $ do
      let from = point 1 3 2
          to   = point 4 (-2) 8
          up   = vector 1 1 0
          t    = SUT.viewTransform from to up
          v    = makeVMatrix [[(-0.50709), 0.50709, 0.67612, (-2.36643)]
                             ,[0.76772, 0.60609, 0.12122, (-2.82843)]
                             ,[-0.35857, 0.59761, (-0.71714), 0.00000]
                             ,[0.00000, 0.00000, 0.00000, 1.00000]]
      it "t = 4x4 translation matrix" $ do
        t `shouldBe` SUT.translation 0 0 (-8)

transformationApplication :: Spec
transformationApplication =
  describe "Application" $ do
    {- Scenario: Individual transformations are applied in sequence
         Given p ← point(1, 0, 1)
           And A ← rotation_x(π / 2)
           And B ← scaling(5, 5, 5)
           And C ← translation(10, 5, 7)
         # apply rotation first
         When p2 ← A * p
         Then p2 = point(1, -1, 0)
         # then apply scaling
         When p3 ← B * p2
         Then p3 = point(5, -5, 0)
         # then apply translation
         When p4 ← C * p3
         Then p4 = point(15, 0, 7) -}
    describe "Individual transformations are applied in sequence" $ do
      let p  = point 1 0 1
          a  = SUT.rotationX (pi/2)
          b  = SUT.scaling 5 5 5
          c  = SUT.translation 10 5 7
          p2 = mulTV a p
          p3 = mulTV b p2
          p4 = mulTV c p3
      it "apply rotation first; p2 = point(1, -1, 0)" $ do
        p2 `shouldBe` point 1 (-1) 0
      it "then apply scaling; p3 = point(5, -5, 0)" $ do
        p3 `shouldBe` point 5 (-5) 0
      it "then apply translation; p4 = point(15, 0, 7)" $ do
        p4 `shouldBe` point 15 0 7
    {- Scenario: Chained transformations must be applied in reverse order
         Given p ← point(1, 0, 1)
           And A ← rotation_x(π / 2)
           And B ← scaling(5, 5, 5)
           And C ← translation(10, 5, 7)
         When T ← C * B * A
         Then T * p = point(15, 0, 7) -}
    describe "Chained transformations must be applied in reverse order" $ do
      let p = point 1 0 1
          t = SUT.transform [SUT.rotationX (pi/2),
                             SUT.scaling 5 5 5,
                             SUT.translation 10 5 7]
      it "T * p = point(15, 0, 7)" $ do
        mulTV t p `shouldBe` point 15 0 7

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
        mulTV t p `shouldBe` point 5 3 4
    {- Scenario: A shearing transformation moves x in proportion to z
         Given transform ← shearing(0, 1, 0, 0, 0, 0)
           And p ← point(2, 3, 4)
         Then transform * p = point(6, 3, 4) -}
    describe "A shearing transformation moves x in proportion to z" $ do
      let t = SUT.shearing 0 1 0 0 0 0
          p = point 2 3 4
      it "transform * p = point(6, 3, 4)" $ do
        mulTV t p `shouldBe` point 6 3 4
    {- Scenario: A shearing transformation moves y in proportion to x
         Given transform ← shearing(0, 0, 1, 0, 0, 0)
           And p ← point(2, 3, 4)
         Then transform * p = point(2, 5, 4) -}
    describe "A shearing transformation moves y in proportion to x" $ do
      let t = SUT.shearing 0 0 1 0 0 0
          p = point 2 3 4
      it "transform * p = point(2, 5, 4)" $ do
        mulTV t p `shouldBe` point 2 5 4
    {- Scenario: A shearing transformation moves y in proportion to z
         Given transform ← shearing(0, 0, 0, 1, 0, 0)
           And p ← point(2, 3, 4)
         Then transform * p = point(2, 7, 4) -}
    describe "A shearing transformation moves y in proportion to z" $ do
      let t = SUT.shearing 0 0 0 1 0 0
          p = point 2 3 4
      it "transform * p = point(2, 7, 4)" $ do
        mulTV t p `shouldBe` point 2 7 4
    {- Scenario: A shearing transformation moves z in proportion to x
         Given transform ← shearing(0, 0, 0, 0, 1, 0)
           And p ← point(2, 3, 4)
         Then transform * p = point(2, 3, 6) -}
    describe "A shearing transformation moves z in proportion to x" $ do
      let t = SUT.shearing 0 0 0 0 1 0
          p = point 2 3 4
      it "transform * p = point(2, 3, 6)" $ do
        mulTV t p `shouldBe` point 2 3 6
    {- Scenario: A shearing transformation moves z in proportion to y
         Given transform ← shearing(0, 0, 0, 0, 0, 1)
           And p ← point(2, 3, 4)
         Then transform * p = point(2, 3, 7) -}
    describe "A shearing transformation moves z in proportion to y" $ do
      let t = SUT.shearing 0 0 0 0 0 1
          p = point 2 3 4
      it "transform * p = point(2, 3, 7)" $ do
        mulTV t p `shouldBe` point 2 3 7

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
        mulTV hq p `shouldBe` point 0 (sqrt 2 / 2) (sqrt 2 / 2)
      it "full_quarter * p = point(0, 0, 1)" $ do
        mulTV fq p `shouldBe` point 0 0 1
    {- Scenario: The inverse of an x-rotation rotates in the opposite direction
         Given p ← point(0, 1, 0)
           And half_quarter ← rotation_x(π / 4)
           And inv ← inverse(half_quarter)
         Then inv * p = point(0, √2/2, -√2/2) -}
    describe "The inverse of an x-rotation rotates in the opposite direction" $ do
      let p  = point 0 1 0
          hq = SUT.rotationX (pi/4)
          i  = inverseV hq
      it "inv * p = point(0, √2/2, -√2/2)" $ do
        mulTV i p `shouldBe` point 0 (sqrt 2 / 2) (-(sqrt 2 / 2))
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
        mulTV hq p `shouldBe` point (sqrt 2 / 2) 0 (sqrt 2 / 2)
      it "full_quarter * p = point(1, 0, 0)" $ do
        mulTV fq p `shouldBe` point 1 0 0
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
        mulTV hq p `shouldBe` point (-(sqrt 2 / 2)) (sqrt 2 / 2) 0
      it "full_quarter * p = point(-1, 0, 0)" $ do
        mulTV fq p `shouldBe` point (-1) 0 0

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
        mulTV t p `shouldBe` point (-8) 18 32
    {- Scenario: A scaling matrix applied to a vector
         Given transform ← scaling(2, 3, 4)
           And v ← vector(-4, 6, 8)
         Then transform * v = vector(-8, 18, 32) -}
    describe "A scaling matrix applied to a vector" $ do
      let t = SUT.scaling 2 3 4
          v = vector (-4) 6 8
      it "transform * v = vector(-8, 18, 32)" $ do
        mulTV t v `shouldBe` vector (-8) 18 32
    {- Scenario: Multiplying by the inverse of a scaling matrix
         Given transform ← scaling(2, 3, 4)
           And inv ← inverse(transform)
           And v ← vector(-4, 6, 8)
         Then inv * v = vector(-2, 2, 2) -}
    describe "Multiplying by the inverse of a scaling matrix" $ do
      let t = SUT.scaling 2 3 4
          i = inverseV t
          v = vector (-4) 6 8
      it "inv * v = vector(-2, 2, 2)" $ do
        mulTV i v `shouldBe` vector (-2) 2 2
    {- Scenario: Reflection is scaling by a negative value
         Given transform ← scaling(-1, 1, 1)
           And p ← point(2, 3, 4)
         Then transform * p = point(-2, 3, 4) -}
    describe "Reflection is scaling by a negative value" $ do
      let t = SUT.scaling (-1) 1 1
          p = point 2 3 4
      it "transform * p = point(-2, 3, 4)" $ do
        mulTV t p `shouldBe` point (-2) 3 4

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
        mulTV t p `shouldBe` point 2 1 7
    {- Scenario: Multiplying by the inverse of a translation matrix
         Given transform ← translation(5, -3, 2)
           And inv ← inverse(transform)
           And p ← point(-3, 4, 5)
         Then inv * p = point(-8, 7, 3) -}
    describe "Multiplying by the inverse of a translation matrix" $ do
      let t = SUT.translation 5 (-3) 2
          i = inverseV t
          p = point (-3) 4 5
      it "inv * p = point(-8, 7, 3)" $ do
         mulTV i p `shouldBe` point (-8) 7 3
    {- Scenario: Translation does not affect vectors
         Given transform ← translation(5, -3, 2)
           And v ← vector(-3, 4, 5)
         Then transform * v = v -}
    describe "Translation does not affect vectors" $ do
      let t = SUT.translation 5 (-3) 2
          v = vector (-3) 4 5
      it "transform * V = V" $ do
        mulTV t v `shouldBe` v
