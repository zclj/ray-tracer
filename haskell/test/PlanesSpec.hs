module PlanesSpec where

import System.IO.Unsafe (unsafePerformIO)

import Test.Tasty
import Test.Tasty.Hspec as HS

import Shapes
import Tuples as T
import Rays
import Types

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
      let p  = defaultPlane 1
          r  = makeRay (T.point 0 10 0) (vector 0 0 1)
          xs = localIntersect p r
      it "intersections are empty" $ do
        xs `shouldBe` []
    {- Scenario: Intersect with a coplanar ray
         Given p ← plane()
           And r ← ray(point(0, 0, 0), vector(0, 0, 1))
         When xs ← local_intersect(p, r)
         Then xs is empty -}
    describe "Intersect with a coplanar ray" $ do
      let p  = defaultPlane 1
          r  = makeRay (T.point 0 0 0) (vector 0 0 1)
          xs = localIntersect p r
      it "intersections are empty" $ do
        xs `shouldBe` []
    {- Scenario: A ray intersecting a plane from above
         Given p ← plane()
           And r ← ray(point(0, 1, 0), vector(0, -1, 0))
         When xs ← local_intersect(p, r)
         Then xs.count = 1
           And xs[0].t = 1
           And xs[0].object = p -}
    describe "A ray intersecting a plane from above" $ do
      let p  = defaultPlane 1
          r  = makeRay (T.point 0 1 0) (vector 0 (-1) 0)
          xs = localIntersect p r
      it "one intersection" $ do
        length xs `shouldBe` 1
      it "xs[0].t = 1" $ do
        intersectionT (head xs) `shouldBe` 1
      it "xs[0].object" $ do
        intersectionObject (head xs) `shouldBe` p
    {- Scenario: A ray intersecting a plane from below
         Given p ← plane()
           And r ← ray(point(0, -1, 0), vector(0, 1, 0))
         When xs ← local_intersect(p, r)
         Then xs.count = 1
           And xs[0].t = 1
           And xs[0].object = p -}
    describe "A ray intersecting a plane from below" $ do
      let p  = defaultPlane 1
          r  = makeRay (T.point 0 (-1) 0) (vector 0 1 0)
          xs = localIntersect p r
      it "one intersection" $ do
        length xs `shouldBe` 1
      it "xs[0].t = 1" $ do
        intersectionT (head xs) `shouldBe` 1
      it "xs[0].object" $ do
        intersectionObject (head xs) `shouldBe` p

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
      let p  = defaultPlane 1
          n1 = localNormalAt p (T.point 0 0 0) (Intersection 0 p)
          n2 = localNormalAt p (T.point 10 0 (-10)) (Intersection 0 p)
          n3 = localNormalAt p (T.point (-5) 0 150) (Intersection 0 p)
      it "n1 = vector(0, 1, 0)" $ do
        n1 `shouldBe` vector 0 1 0
      it "n2 = vector(0, 1, 0)" $ do
        n2 `shouldBe` vector 0 1 0
      it "n3 = vector(0, 1, 0)" $ do
        n3 `shouldBe` vector 0 1 0
