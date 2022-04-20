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
