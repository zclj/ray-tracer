module CubesSpec where

import System.IO.Unsafe (unsafePerformIO)

import Test.Tasty
import Test.Tasty.Hspec as HS

import Types
import Shapes
import Tuples as T
import Materials
import Rays

cubeTests :: TestTree
cubeTests = testGroup "Cubes Tests" [
  testGroup "Specs for"
  [ unsafePerformIO (testSpec "Cubes" cubeIntersections)]]

cubeIntersections :: Spec
cubeIntersections =
  describe "Cubes" $ do
    {- Scenario Outline: A ray intersects a cube
         Given c ← cube()
           And r ← ray(<origin>, <direction>)
         When xs ← local_intersect(c, r)
         Then xs.count = 2
           And xs[0].t = <t1>
           And xs[1].t = <t2>
         Examples:
           |        | origin            | direction        | t1 | t2 |
           | +x     | point(5, 0.5, 0)  | vector(-1, 0, 0) |  4 |  6 |
           | -x     | point(-5, 0.5, 0) | vector(1, 0, 0)  |  4 |  6 |
           | +y     | point(0.5, 5, 0)  | vector(0, -1, 0) |  4 |  6 |
           | -y     | point(0.5, -5, 0) | vector(0, 1, 0)  |  4 |  6 |
           | +z     | point(0.5, 0, 5)  | vector(0, 0, -1) |  4 |  6 |
           | -z     | point(0.5, 0, -5) | vector(0, 0, 1)  |  4 |  6 |
           | inside | point(0, 0.5, 0)  | vector(0, 0, 1)  | -1 |  1 | -}
    describe "A ray intersects a cube" $ do
      let c = defaultCube 1
          origins = [ T.point 5 0.5 0
                    , T.point (-5) 0.5 0
                    , T.point 0.5 5 0
                    , T.point 0.5 (-5) 0
                    , T.point 0.5 0 5
                    , T.point 0.5 0 (-5)
                    , T.point 0 0.5 0]
          direction = [ T.vector (-1) 0 0
                      , T.vector 1 0 0
                      , T.vector 0 (-1) 0
                      , T.vector 0 1 0
                      , T.vector 0 0 (-1)
                      , T.vector 0 0 1
                      , T.vector 0 0 1]
          t1s = [4, 4, 4, 4, 4, 4, (-1)]
          t2s = [6, 6, 6, 6, 6, 6, 1]
          rays = zipWith makeRay origins direction
          xs = map (localIntersect c) rays
      it "t1s intersect" $ do
       map (\[(Intersection t _), _] -> t) xs `shouldBe` t1s
      it "t2s intersect" $ do
       map (\[_, (Intersection t _)] -> t) xs `shouldBe` t2s
    {- Scenario Outline: A ray misses a cube
         Given c ← cube()
           And r ← ray(<origin>, <direction>)
         When xs ← local_intersect(c, r)
         Then xs.count = 0
         Examples:
           | origin           | direction                      |
           | point(-2, 0, 0)  | vector(0.2673, 0.5345, 0.8018) |
           | point(0, -2, 0)  | vector(0.8018, 0.2673, 0.5345) |
           | point(0, 0, -2)  | vector(0.5345, 0.8018, 0.2673) |
           | point(2, 0, 2)   | vector(0, 0, -1)               |
           | point(0, 2, 2)   | vector(0, -1, 0)               |
           | point(2, 2, 0)   | vector(-1, 0, 0)               | -}
    describe "Outline: A ray misses a cube" $ do
      let c = defaultCube 1
          origins = [ T.point (-2) 0 0
                    , T.point 0 (-2) 0
                    , T.point 0 0 (-2)
                    , T.point 2 0 2
                    , T.point 0 2 2
                    , T.point 2 2 0]
          direction = [ T.vector 0.2673 0.5345 0.8018
                      , T.vector 0.8018 0.2673 0.5345
                      , T.vector 0.5345 0.8018 0.2673
                      , T.vector 0 0 (-1)
                      , T.vector 0 (-1) 0
                      , T.vector (-1) 0 0]
          rays = zipWith makeRay origins direction
          xs = map (localIntersect c) rays
      it "no intersections" $ do
       map (\is -> length is) xs `shouldBe` [0, 0, 0, 0, 0, 0]
    {- Scenario Outline: The normal on the surface of a cube
         Given c ← cube()
           And p ← <point>
         When normal ← local_normal_at(c, p)
         Then normal = <normal>

         Examples:
           | point                | normal           |
           | point(1, 0.5, -0.8)  | vector(1, 0, 0)  |
           | point(-1, -0.2, 0.9) | vector(-1, 0, 0) |
           | point(-0.4, 1, -0.1) | vector(0, 1, 0)  |
           | point(0.3, -1, -0.7) | vector(0, -1, 0) |
           | point(-0.6, 0.3, 1)  | vector(0, 0, 1)  |
           | point(0.4, 0.4, -1)  | vector(0, 0, -1) |
           | point(1, 1, 1)       | vector(1, 0, 0)  |
           | point(-1, -1, -1)    | vector(-1, 0, 0) | -}
    describe "The normal on the surface of a cube" $ do
      let c = defaultCube 1
          points = [ T.point 1 0.5 (-0.8)
                   , T.point (-1) (-0.2) 0.9
                   , T.point (-0.4) 1 (-0.1)
                   , T.point (0.3) (-1) (-0.7)
                   , T.point (-0.6) 0.3 1
                   , T.point 0.4 0.4 (-1)
                   , T.point 1 1 1
                   , T.point (-1) (-1) (-1)]
          normals = [ T.vector 1 0 0
                    , T.vector (-1) 0 0
                    , T.vector 0 1 0
                    , T.vector 0 (-1) 0
                    , T.vector 0 0 1
                    , T.vector 0 0 (-1)
                    , T.vector 1 0 0
                    , T.vector (-1) 0 0]
          ns = map (localNormalAt c) points
      it "is correct" $ do
        ns `shouldBe` normals
