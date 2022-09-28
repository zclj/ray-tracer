module ConesSpec where

import System.IO.Unsafe (unsafePerformIO)

import Test.Tasty
import Test.Tasty.Hspec as HS

import Types
import Shapes
import Tuples as T
import Rays

coneTests :: TestTree
coneTests = testGroup "Cone Tests" [
  testGroup "Specs for"
  [ unsafePerformIO (testSpec "Cones" coneIntersections)
  --, unsafePerformIO (testSpec "Cones" coneNormal)
  ]]

coneIntersections :: Spec
coneIntersections =
  describe "Cones" $ do
    {- Scenario Outline: Intersecting a cone with a ray
         Given shape ← cone()
           And direction ← normalize(<direction>)
           And r ← ray(<origin>, direction)
         When xs ← local_intersect(shape, r)
         Then xs.count = 2
           And xs[0].t = <t0>
           And xs[1].t = <t1>

         Examples:
           | origin          | direction           | t0      | t1       |
           | point(0, 0, -5) | vector(0, 0, 1)     | 5       |  5       |
           | point(0, 0, -5) | vector(1, 1, 1)     | 8.66025 |  8.66025 |
           | point(1, 1, -5) | vector(-0.5, -1, 1) | 4.55006 | 49.44994 | -}
    describe "Intersecting a cone with a ray" $ do
      let s          = defaultCone 1
          origins    = [ T.point 0 0 (-5)
                       , T.point 0 0 (-5)
                       , T.point 1 1 (-5)]
          directions = [ T.vector 0 0 1
                       , T.vector 1 1 1
                       , T.vector (-0.5) (-1) 1]
          rays       = zipWith makeRay origins directions
          t0s        = [5, 8.66025, 4.55006]
          t1s        = [5, 8.66025, 49.44994]
          xs         = map (localIntersect s) rays
          getT0 [(Intersection t _), _] = t
          getT1 [_, (Intersection t _)] = t
      it "all ray hits" $ do
        map length xs `shouldBe` [2, 2, 2]
      it "t0s intersect" $ do
        map getT0 xs `shouldBe` t0s
      it "t1s intersect" $ do
        map getT1 xs `shouldBe` t1s
