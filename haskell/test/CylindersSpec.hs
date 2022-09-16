module CylindersSpec where

import System.IO.Unsafe (unsafePerformIO)

import Test.Tasty
import Test.Tasty.Hspec as HS

import Types
import Shapes
import Tuples as T
import Rays

cylinderTests :: TestTree
cylinderTests = testGroup "Cylinder Tests" [
  testGroup "Specs for"
  [ unsafePerformIO (testSpec "Cylinders" cylinderIntersections)]]

cylinderIntersections :: Spec
cylinderIntersections =
  describe "Cylinders" $ do
    {- Scenario Outline: A ray misses a cylinder
         Given cyl ← cylinder()
           And direction ← normalize(<direction>)
           And r ← ray(<origin>, direction)
         When xs ← local_intersect(cyl, r)
         Then xs.count = 0

         Examples:
           | origin          | direction       |
           | point(1, 0, 0)  | vector(0, 1, 0) |
           | point(0, 0, 0)  | vector(0, 1, 0) |
           | point(0, 0, -5) | vector(1, 1, 1) | -}
    describe "A ray misses a cylinder" $ do
      let c          = defaultCylinder 1
          origins    = [ T.point 1 0 0
                       , T.point 0 0 0
                       , T.point 0 0 (-5)]
          directions = [ T.vector 0 1 0
                       , T.vector 0 1 0
                       , T.vector 1 1 1]
          rays       = zipWith makeRay origins directions
          xs         = map (localIntersect c) rays
      it "all rays misses" $ do
        map length xs `shouldBe` [0, 0, 0]
