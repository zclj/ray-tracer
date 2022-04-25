module IntersectionsSpec where

import System.IO.Unsafe (unsafePerformIO)

import Test.Tasty
import Test.Tasty.Hspec as HS

import Spheres
import Intersections as SUT

intersectionsTests :: TestTree
intersectionsTests = testGroup "Intersections Tests" [
  testGroup "Specs for"
  [ unsafePerformIO (testSpec "Intersections" intersections)]]

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
          i = (SUT.Intersection 3.5 s)
      it "t of intersection is 3.5" $ do
        (t i) `shouldBe` 3.5
      it "object is the sphere" $ do
        (object i) `shouldBe` s
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
