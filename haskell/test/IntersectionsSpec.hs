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
