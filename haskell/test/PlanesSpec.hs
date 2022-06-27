module PlanesSpec where

import System.IO.Unsafe (unsafePerformIO)

import Test.Tasty
import Test.Tasty.Hspec as HS

import Shapes
import Tuples
import Rays
import Planes as SUT

planesTests :: TestTree
planesTests = testGroup "Planes Tests" [
  testGroup "Specs for"
  [ unsafePerformIO (testSpec "Planes" planeNormal)
  , unsafePerformIO (testSpec "Planes" planeIntersections)]]

planeIntersections :: Spec
planeIntersections =
  describe "Intersections" $ do
    {- Scenario: Intersect with a ray parallel to the plane
         Given p ← plane()
           And r ← ray(point(0, 10, 0), vector(0, 0, 1))
         When xs ← local_intersect(p, r)
         Then xs is empty -}
    describe "Intersect with a ray parallel to the plane" $ do
      let p  = SUT.makePlane 1
          r  = makeRay (point 0 10 0) (vector 0 0 1)
          xs = shapeIntersect p r
      it "intersections are empty" $ do
        xs `shouldBe` []

planeNormal :: Spec
planeNormal =
  describe "Normals" $ do
    {- Scenario: The normal of a plane is constant everywhere
         Given p ← plane()
         When n1 ← local_normal_at(p, point(0, 0, 0))
           And n2 ← local_normal_at(p, point(10, 0, -10))
           And n3 ← local_normal_at(p, point(-5, 0, 150))
         Then n1 = vector(0, 1, 0)
           And n2 = vector(0, 1, 0)
           And n3 = vector(0, 1, 0) -}
    describe "The normal of a plane is constant everywhere" $ do
      let p  = SUT.makePlane 1
          n1 = shapeNormalAt p (point 0 0 0)
          n2 = shapeNormalAt p (point 10 0 (-10))
          n3 = shapeNormalAt p (point (-5) 0 150)
      it "n1 = vector(0, 1, 0)" $ do
        n1 `shouldBe` vector 0 1 0
      it "n2 = vector(0, 1, 0)" $ do
        n2 `shouldBe` vector 0 1 0
      it "n3 = vector(0, 1, 0)" $ do
        n3 `shouldBe` vector 0 1 0
