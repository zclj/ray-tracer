module SpheresSpec where

import System.IO.Unsafe (unsafePerformIO)

import Test.Tasty
import Test.Tasty.Hspec as HS

import Tuples
import Rays
import Spheres as SUT

spheresTests :: TestTree
spheresTests = testGroup "Spheres Tests" [
  testGroup "Specs for"
  [ unsafePerformIO (testSpec "Spheres" sphereIntersections)]]

sphereIntersections :: Spec
sphereIntersections =
  describe "Intersections" $ do
    {- Scenario: A ray intersects a sphere at two points
         Given r ← ray(point(0, 0, -5), vector(0, 0, 1))
           And s ← sphere()
         When xs ← intersect(s, r)
         Then xs.count = 2
           And xs[0] = 4.0
           And xs[1] = 6.0 -}
    describe "A ray intersects a sphere at two points" $ do
      let r = makeRay (point 0 0 (-5)) (vector 0 0 1)
          s = SUT.makeUnitSphere 1
          xs = SUT.intersects s r
      it "there are two intersections" $ do
        length xs `shouldBe` 2
      it "intersection one is 4.0" $ do
        (head xs) `shouldBe` 4.0
      it "intersection two is 6.0" $ do
        (last xs) `shouldBe` 6.0
    {- Scenario: A ray intersects a sphere at a tangent
         Given r ← ray(point(0, 1, -5), vector(0, 0, 1))
           And s ← sphere()
         When xs ← intersect(s, r)
         Then xs.count = 2
           And xs[0] = 5.0
           And xs[1] = 5.0 -}
    describe "A ray intersects a sphere at a tangent" $ do
      let r = makeRay (point 0 1 (-5)) (vector 0 0 1)
          s = SUT.makeUnitSphere 1
          xs = SUT.intersects s r
      it "there are two intersections" $ do
        length xs `shouldBe` 2
      it "intersection one is 5.0" $ do
        (head xs) `shouldBe` 5.0
      it "intersection two is 5.0" $ do
        (last xs) `shouldBe` 5.0
    {- Scenario: A ray misses a sphere
         Given r ← ray(point(0, 2, -5), vector(0, 0, 1))
           And s ← sphere()
         When xs ← intersect(s, r)
         Then xs.count = 0 -}
    describe "A ray misses a sphere" $ do
      let r = makeRay (point 0 2 (-5)) (vector 0 0 1)
          s = SUT.makeUnitSphere 1
          xs = SUT.intersects s r
      it "there are no intersections" $ do
        length xs `shouldBe` 0
    {- Scenario: A ray originates inside a sphere
         Given r ← ray(point(0, 0, 0), vector(0, 0, 1))
           And s ← sphere()
         When xs ← intersect(s, r)
         Then xs.count = 2
           And xs[0] = -1.0
           And xs[1] = 1.0 -}
    describe "A ray originates inside a sphere" $ do
      let r = makeRay (point 0 0 0) (vector 0 0 1)
          s = SUT.makeUnitSphere 1
          xs = SUT.intersects s r
      it "there are two intersections" $ do
        length xs `shouldBe` 2
      it "intersection one is -1.0" $ do
        (head xs) `shouldBe` (-1.0)
      it "intersection two is 1.0" $ do
        (last xs) `shouldBe` 1.0
