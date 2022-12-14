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
  [ unsafePerformIO (testSpec "Cylinders" cylinderIntersections)
  , unsafePerformIO (testSpec "Cylinders" cylinderNormal)]]

cylinderNormal :: Spec
cylinderNormal =
  describe "Cylinders" $ do
    {- Scenario Outline: Normal vector on a cylinder
         Given cyl ← cylinder()
         When n ← local_normal_at(cyl, <point>)
         Then n = <normal>

         Examples:
           | point           | normal           |
           | point(1, 0, 0)  | vector(1, 0, 0)  |
           | point(0, 5, -1) | vector(0, 0, -1) |
           | point(0, -2, 1) | vector(0, 0, 1)  |
           | point(-1, 1, 0) | vector(-1, 0, 0) | -}
    describe "Normal vector on a cylinder" $ do
      let cyl  = defaultCylinder 1
          points = [ T.point 1 0 0
                    , T.point 0 5 (-1)
                    , T.point 0 (-2) 1
                    , T.point (-1) 1 0]
          normals = [ T.vector 1 0 0
                     , T.vector 0 0 (-1)
                     , T.vector 0 0 1
                     , T.vector (-1) 0 0]
          ns   = map (\t -> localNormalAt cyl t (Intersection 0 cyl)) points
      it "all normals are calculated" $ do
        ns `shouldBe` normals
    {- Scenario Outline: The normal vector on a cylinder's end caps
         Given cyl ← cylinder()
           And cyl.minimum ← 1
           And cyl.maximum ← 2
           And cyl.closed ← true
         When n ← local_normal_at(cyl, <point>)
         Then n = <normal>

         Examples:
           | point            | normal           |
           | point(0, 1, 0)   | vector(0, -1, 0) |
           | point(0.5, 1, 0) | vector(0, -1, 0) |
           | point(0, 1, 0.5) | vector(0, -1, 0) |
           | point(0, 2, 0)   | vector(0, 1, 0)  |
           | point(0.5, 2, 0) | vector(0, 1, 0)  |
           | point(0, 2, 0.5) | vector(0, 1, 0)  | -}
    describe "The normal vector on a cylinder's end caps" $ do
      let cyl  = (defaultCylinder 1) { minY = 1, maxY = 2 }
          points = [ T.point 0 1 0
                   , T.point 0.5 1 0
                   , T.point 0 1 0.5
                   , T.point 0 2 0
                   , T.point 0.5 2 0
                   , T.point 0 2 0.5]
          normals = [ T.vector 0 (-1) 0
                    , T.vector 0 (-1) 0
                    , T.vector 0 (-1) 0
                    , T.vector 0 1 0
                    , T.vector 0 1 0
                    , T.vector 0 1 0]
          ns   = map (\t -> localNormalAt cyl t (Intersection 0 cyl)) points
      it "all normals are calculated" $ do
        ns `shouldBe` normals

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
    {- Scenario Outline: A ray strikes a cylinder
         Given cyl ← cylinder()
           And direction ← normalize(<direction>)
           And r ← ray(<origin>, direction)
         When xs ← local_intersect(cyl, r)
         Then xs.count = 2
           And xs[0].t = <t0>
           And xs[1].t = <t1>

         Examples:
           | origin            | direction         | t0      | t1      |
           | point(1, 0, -5)   | vector(0, 0, 1)   | 5       | 5       |
           | point(0, 0, -5)   | vector(0, 0, 1)   | 4       | 6       |
           | point(0.5, 0, -5) | vector(0.1, 1, 1) | 6.80798 | 7.08872 | -}
    describe "A ray strikes a cylinder" $ do
      let cyl        = defaultCylinder 1
          origins    = [ T.point 1 0 (-5)
                       , T.point 0 0 (-5)
                       , T.point 0.5 0 (-5)]
          directions = map T.norm [ T.vector 0 0 1
                                  , T.vector 0 0 1
                                  , T.vector 0.1 1 1]
          rays       = zipWith makeRay origins directions
          t0s        = [5, 4, 6.80798]
          t1s        = [5, 6, 7.08872]
          xs         = map (localIntersect cyl) rays
          getT0 [(Intersection t _), _] = t
          getT1 [_, (Intersection t _)] = t
      it "all ray hits" $ do
        map length xs `shouldBe` [2, 2, 2]
      it "t0 - 1 intersect" $ do
        getT0 (xs !! 0) `shouldBe` t0s !! 0
      it "t0 - 2 intersect" $ do
        getT0 (xs !! 1) `shouldBe` t0s !! 1
      it "t0 - 3 intersect" $ do
        getT0 (xs !! 2) ~= (t0s !! 2) `shouldBe` True
      it "t1 - 1 intersect" $ do
        getT1 (xs !! 0) `shouldBe` t1s !! 0
      it "t1 - 2 intersect" $ do
        getT1 (xs !! 1) `shouldBe` t1s !! 1
      it "t1 - 3 intersect" $ do
        getT1 (xs !! 2) ~= (t1s !! 2) `shouldBe` True
    {- Scenario: The default minimum and maximum for a cylinder
         Given cyl ← cylinder()
         Then cyl.minimum = -infinity
         And cyl.maximum = infinity -}
    describe "The default minimum and maximum for a cylinder" $ do
      let cyl = defaultCylinder 1
      it "default minimum is -infinity" $ do
        minY cyl `shouldBe` -1/0
      it "default maximum is infinity" $ do
        maxY cyl `shouldBe` 1/0
    {- Scenario Outline: Intersecting a constrained cylinder
         Given cyl ← cylinder()
           And cyl.minimum ← 1
           And cyl.maximum ← 2
           And direction ← normalize(<direction>)
           And r ← ray(<point>, direction)
         When xs ← local_intersect(cyl, r)
         Then xs.count = <count>

         Examples:
           |   | point             | direction         | count |
           | 1 | point(0, 1.5, 0)  | vector(0.1, 1, 0) | 0     |
           | 2 | point(0, 3, -5)   | vector(0, 0, 1)   | 0     |
           | 3 | point(0, 0, -5)   | vector(0, 0, 1)   | 0     |
           | 4 | point(0, 2, -5)   | vector(0, 0, 1)   | 0     |
           | 5 | point(0, 1, -5)   | vector(0, 0, 1)   | 0     |
           | 6 | point(0, 1.5, -2) | vector(0, 0, 1)   | 2     | -}
    describe "Intersecting a constrained cylinder" $ do
      let cyl = (defaultCylinder 1) {minY = 1, maxY = 2}
          origins    = [ T.point 0 1.5 0
                        , T.point 0 3 (-5)
                        , T.point 0 0 (-5)
                        , T.point 0 2 (-5)
                        , T.point 0 1 (-5)
                        , T.point 0 1.5 (-2)]
          directions = map T.norm [ T.vector 0.1 1 0
                                    , T.vector 0 0 1
                                    , T.vector 0 0 1
                                    , T.vector 0 0 1
                                    , T.vector 0 0 1
                                    , T.vector 0 0 1]
          cs         = [0, 0, 0, 0, 0, 2]
          xs         = map (localIntersect cyl) rays
          rays       = zipWith makeRay origins directions
      it "intersects" $ do
        map length xs `shouldBe` cs
    {- Scenario: The default closed value for a cylinder
         Given cyl ← cylinder()
         Then cyl.closed = false -}
    describe "The default closed value for a cylinder" $ do
      let cyl = defaultCylinder 1
      it "is false" $ do
        closed cyl `shouldBe` False
    {- Scenario Outline: Intersecting the caps of a closed cylinder
         Given cyl ← cylinder()
           And cyl.minimum ← 1
           And cyl.maximum ← 2
           And cyl.closed ← true
           And direction ← normalize(<direction>)
           And r ← ray(<point>, direction)
         When xs ← local_intersect(cyl, r)
         Then xs.count = <count>

         Examples:
           |   | point            | direction        | count |
           | 1 | point(0, 3, 0)   | vector(0, -1, 0) | 2     |
           | 2 | point(0, 3, -2)  | vector(0, -1, 2) | 2     |
           | 3 | point(0, 4, -2)  | vector(0, -1, 1) | 2     | # corner case
           | 4 | point(0, 0, -2)  | vector(0, 1, 2)  | 2     |
           | 5 | point(0, -1, -2) | vector(0, 1, 1)  | 2     | # corner case -}
    describe "Intersecting the caps of a closed cylinder" $ do
      let cyl = (defaultCylinder 1) {minY = 1, maxY = 2, closed = True}
          origins    = [ T.point 0 3 0
                       , T.point 0 3 (-2)
                       , T.point 0 4 (-2)
                       , T.point 0 0 (-2)
                       , T.point 0 (-1) (-2)]
          directions = map T.norm [ T.vector 0 (-1) 0
                                  , T.vector 0 (-1) 2
                                  , T.vector 0 (-1) 1
                                  , T.vector 0 1 2
                                  , T.vector 0 1 1]
          cs         = [2, 2, 2, 2, 2]
          xs         = map (localIntersect cyl) rays
          rays       = zipWith makeRay origins directions
      it "intersects" $ do
        map length xs `shouldBe` cs
