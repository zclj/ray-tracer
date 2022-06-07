module IntersectionsSpec where

import System.IO.Unsafe (unsafePerformIO)

import Test.Tasty
import Test.Tasty.Hspec as HS

import Spheres
import Rays
import Tuples
import qualified Computation
import Intersections as SUT

intersectionsTests :: TestTree
intersectionsTests = testGroup "Intersections Tests" [
  testGroup "Specs for"
  [ unsafePerformIO (testSpec "Intersections" intersections)
  , unsafePerformIO (testSpec "Intersections" hits)
  , unsafePerformIO (testSpec "Intersections" precompute)]]

precompute :: Spec
precompute =
  describe "Precomputations" $ do
    {- Scenario: Precomputing the state of an intersection
         Given r ← ray(point(0, 0, -5), vector(0, 0, 1))
           And shape ← sphere()
           And i ← intersection(4, shape)
         When comps ← prepare_computations(i, r)
         Then comps.t = i.t
           And comps.object = i.object
           And comps.point = point(0, 0, -1)
           And comps.eyev = vector(0, 0, -1)
           And comps.normalv = vector(0, 0, -1) -}
    describe "Precomputing the state of an intersection" $ do
      let r = makeRay (point 0 0 (-5)) (vector 0 0 1)
          shape = makeUnitSphere 1
          i = SUT.Intersection 4 shape
          comps = SUT.prepareComputations i r
      it "computation t = i.t" $ do
        (Computation.t comps) `shouldBe` (t i)

intersections :: Spec
intersections =
  describe "Basic Intersections" $ do
    {- Scenario: An intersection encapsulates t and object
         Given s ← sphere()
         When i ← intersection(3.5, s)
         Then i.t = 3.5
           And i.object = s -}
    describe "An intersection encapsulates t and object" $ do
      let s = makeUnitSphere 1
          i = SUT.Intersection 3.5 s
      it "t of intersection is 3.5" $ do
        t i `shouldBe` 3.5
      it "object is the sphere" $ do
        object i `shouldBe` s
    {- Scenario: Aggregating intersections
         Given s ← sphere()
           And i1 ← intersection(1, s)
           And i2 ← intersection(2, s)
         When xs ← intersections(i1, i2)
         Then xs.count = 2
           And xs[0].t = 1
           And xs[1].t = 2 -}
    describe "Aggregating intersections" $ do
      let s = makeUnitSphere 1
          i1 = SUT.Intersection 1 s
          i2 = SUT.Intersection 2 s
          -- Feature says to use a type 'Intersections' but it seems to be just a
          -- list for now. Change if needed
          xs = [i1, i2]
      it "the number of intersections is 2" $ do
        length xs `shouldBe` 2
      it "the first intersections t is 1" $ do
        t (head xs) `shouldBe` 1
      it "the second intersections t is 2" $ do
        t (last xs) `shouldBe` 2

hits :: Spec
hits =
  describe "Intersection hits" $ do
    {- Scenario: The hit, when all intersections have positive t
         Given s ← sphere()
           And i1 ← intersection(1, s)
           And i2 ← intersection(2, s)
           And xs ← intersections(i2, i1)
         When i ← hit(xs)
         Then i = i1 -}
    describe "The hit, when all intersections have positive t" $ do
      let s  = makeUnitSphere 1
          i1 = SUT.Intersection 1 s
          i2 = SUT.Intersection 2 s
          xs = [i1, i2]
          i  = SUT.hit xs
      it "The hit is the first positive intersection" $ do
        i `shouldBe` Just i1
    {- Scenario: The hit, when some intersections have negative t
         Given s ← sphere()
           And i1 ← intersection(-1, s)
           And i2 ← intersection(1, s)
           And xs ← intersections(i2, i1)
         When i ← hit(xs)
         Then i = i2 -}
    describe "The hit, when some intersections have negative t" $ do
      let s  = makeUnitSphere 1
          i1 = SUT.Intersection (-1) s
          i2 = SUT.Intersection 1 s
          xs = [i2, i1]
          i  = SUT.hit xs
      it "The hit is the first positive intersection" $ do
        i `shouldBe` Just i2
    {- Scenario: The hit, when all intersections have negative t
         Given s ← sphere()
           And i1 ← intersection(-2, s)
           And i2 ← intersection(-1, s)
           And xs ← intersections(i2, i1)
         When i ← hit(xs)
         Then i is nothing -}
    describe "The hit, when all intersections have negative t" $ do
      let s  = makeUnitSphere 1
          i1 = SUT.Intersection (-2) s
          i2 = SUT.Intersection (-1) s
          xs = [i2, i1]
          i  = SUT.hit xs
      it "The hit is the first positive intersection" $ do
        i `shouldBe` Nothing
    {- Scenario: The hit is always the lowest nonnegative intersection
         Given s ← sphere()
           And i1 ← intersection(5, s)
           And i2 ← intersection(7, s)
           And i3 ← intersection(-3, s)
           And i4 ← intersection(2, s)
           And xs ← intersections(i1, i2, i3, i4)
         When i ← hit(xs)
         Then i = i4 -}
    describe "The hit is always the lowest nonnegative intersection" $ do
      let s  = makeUnitSphere 1
          i1 = SUT.Intersection 5 s
          i2 = SUT.Intersection 7 s
          i3 = SUT.Intersection (-3) s
          i4 = SUT.Intersection 2 s
          xs = [i1, i2, i3, i4]
          i  = SUT.hit xs
      it "The hit is the first positive intersection" $ do
        i `shouldBe` Just i4
