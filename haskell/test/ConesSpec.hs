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
  , unsafePerformIO (testSpec "Cones" coneNormal)]]

coneNormal :: Spec
coneNormal =
  describe "Cones" $ do
    {- Scenario Outline: Computing the normal vector on a cone
         Given shape ← cone()
         When n ← local_normal_at(shape, <point>)
         Then n = <normal>

         Examples:
           | point             | normal                 |
           | point(0, 0, 0)    | vector(0, 0, 0)        |
           | point(1, 1, 1)    | vector(1, -√2, 1)      |
           | point(-1, -1, 0)  | vector(-1, 1, 0)       | -}
    describe "Computing the normal vector on a cone" $ do
      let s      = defaultCone 1
          points = [ T.point 0 0 0
                   , T.point 1 1 1
                   , T.point (-1) (-1) 0]
          normals = [ T.vector 0 0 0
                    , T.vector 1 (-sqrt 2) 1
                    , T.vector (-1) 1 0]
          ns   = map (\t -> localNormalAt s t (Intersection 0 s)) points
      it "all normals are calculated" $ do
        ns `shouldBe` normals

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
          directions = map T.norm [ T.vector 0 0 1
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
      it "t0 - 1  intersect" $ do
        getT0 (xs !! 0) ~= (t0s !! 0) `shouldBe` True
      it "t0 - 2  intersect" $ do
        getT0 (xs !! 1) ~= (t0s !! 1) `shouldBe` True
      it "t0 - 3  intersect" $ do
        getT0 (xs !! 2) ~= (t0s !! 2) `shouldBe` True
      it "t1 - 1  intersect" $ do
        getT1 (xs !! 0) ~= (t1s !! 0) `shouldBe` True
      it "t1 - 2  intersect" $ do
        getT1 (xs !! 1) ~= (t1s !! 1) `shouldBe` True
      it "t1 - 3  intersect" $ do
        getT1 (xs !! 2) ~= (t1s !! 2) `shouldBe` True
    {- Scenario: Intersecting a cone with a ray parallel to one of its halves
         Given shape ← cone()
           And direction ← normalize(vector(0, 1, 1))
           And r ← ray(point(0, 0, -1), direction)
         When xs ← local_intersect(shape, r)
         Then xs.count = 1
           And xs[0].t = 0.35355 -}
    describe "Intersecting a cone with a ray parallel to one of its halves" $ do
      let s   = defaultCone 1
          dir = T.norm (T.vector 0 1 1)
          r   = makeRay (T.point 0 0 (-1)) dir
          xs  = localIntersect s r
          getT0 [(Intersection t _)] = t
      it "one intersection" $ do
        length xs `shouldBe` 1
      it "correct value" $ do
        getT0 xs ~= 0.35355 `shouldBe` True
    {- Scenario Outline: Intersecting a cone's end caps
         Given shape ← cone()
           And shape.minimum ← -0.5
           And shape.maximum ← 0.5
           And shape.closed ← true
           And direction ← normalize(<direction>)
           And r ← ray(<origin>, direction)
         When xs ← local_intersect(shape, r)
         Then xs.count = <count>

         Examples:
           | origin             | direction       | count |
           | point(0, 0, -5)    | vector(0, 1, 0) | 0     |
           | point(0, 0, -0.25) | vector(0, 1, 1) | 2     |
           | point(0, 0, -0.25) | vector(0, 1, 0) | 4     | -}
    describe "Intersecting a cone's end caps" $ do
      let s          = (defaultCone 1) { minY = (-0.5), maxY = 0.5, closed = True }
          origins    = [ T.point 0 0 (-5)
                       , T.point 0 0 (-0.25)
                       , T.point 0 0 (-0.25)]
          directions = map T.norm [ T.vector 0 1 0
                                  , T.vector 0 1 1
                                  , T.vector 0 1 0]
          rays       = zipWith makeRay origins directions
          cs         = [0, 2, 4]
          xs         = map (localIntersect s) rays
      it "intersection counts" $ do
        map length xs `shouldBe` cs
