module RaysSpec where

import System.IO.Unsafe (unsafePerformIO)

import Test.Tasty
import Test.Tasty.Hspec as HS

import Tuples
import Rays as SUT

raysTests :: TestTree
raysTests = testGroup "Rays Tests" [
  testGroup "Specs for"
  [ unsafePerformIO (testSpec "Rays" raysBasics)]]

raysBasics :: Spec
raysBasics =
  describe "Basics" $ do
    {- Scenario: Creating and querying a ray
         Given origin ← point(1, 2, 3)
           And direction ← vector(4, 5, 6)
         When r ← ray(origin, direction)
         Then r.origin = origin
           And r.direction = direction -}
    describe "Creating and querying a ray" $ do
      let origin = point 1 2 3
          direction = vector 4 5 6
          r = SUT.makeRay origin direction
      it "can query origin" $ do
        SUT.origin r `shouldBe` origin
      it "can query direction" $ do
        SUT.direction r `shouldBe` direction
    {- Scenario: Computing a point from a distance
         Given r ← ray(point(2, 3, 4), vector(1, 0, 0))
         Then position(r, 0) = point(2, 3, 4)
           And position(r, 1) = point(3, 3, 4)
           And position(r, -1) = point(1, 3, 4)
           And position(r, 2.5) = point(4.5, 3, 4) -}
    describe "Computing a point from a distance" $ do
      let r  = SUT.makeRay (point 2 3 4) (vector 1 0 0)
          p1 = position r 0
          p2 = position r 1
          p3 = position r (-1)
          p4 = position r 2.5
      it "position 1 is in point(2, 3, 4)" $ do
        p1 `shouldBe` point 2 3 4
      it "position 2 is in point(3, 3, 4)" $ do
        p2 `shouldBe` point 3 3 4
      it "position 3 is in point(1, 3, 4)" $ do
        p3 `shouldBe` point 1 3 4
      it "position 4 is in point(4.5, 3, 4)" $ do
        p4 `shouldBe` point 4.5 3 4
