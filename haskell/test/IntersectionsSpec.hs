module IntersectionsSpec where

import System.IO.Unsafe (unsafePerformIO)

import Test.Tasty
import Test.Tasty.Hspec as HS
import Rays
import Tuples as T
import Transformations
import Intersection as SUT
import Types
import Materials
import Shapes

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
      let r     = makeRay (T.point 0 0 (-5)) (vector 0 0 1)
          shape = defaultSphere 1
          i     = Intersection 4 shape
          comps = SUT.prepareComputations i r [i]
      it "computation t = i.t" $ do
        t comps `shouldBe` intersectionT i
      it "computation object = i.object" $ do
        object comps `shouldBe` intersectionObject i
      it "computation point = point(0, 0, -1)" $ do
        Types.point comps `shouldBe` T.point 0 0 (-1)
      it "computation eyev = vector(0, 0, -1)" $ do
        eyev comps `shouldBe` vector 0 0 (-1)
      it "computation normalv = vector(0, 0, -1)" $ do
        normalv comps `shouldBe` vector 0 0 (-1)
    {- Scenario: The hit, when an intersection occurs on the outside
         Given r ← ray(point(0, 0, -5), vector(0, 0, 1))
           And shape ← sphere()
           And i ← intersection(4, shape)
         When comps ← prepare_computations(i, r)
         Then comps.inside = false -}
    describe "The hit, when an intersection occurs on the outside" $ do
      let r     = makeRay (T.point 0 0 (-5)) (vector 0 0 1)
          shape = defaultSphere 1
          i     = Intersection 4 shape
          comps = SUT.prepareComputations i r [i]
      it "comps.inside = false" $ do
        inside comps `shouldBe` False
    {- Scenario: The hit, when an intersection occurs on the inside
         Given r ← ray(point(0, 0, 0), vector(0, 0, 1))
           And shape ← sphere()
           And i ← intersection(1, shape)
         When comps ← prepare_computations(i, r)
         Then comps.point = point(0, 0, 1)
           And comps.eyev = vector(0, 0, -1)
           And comps.inside = true
           # normal would have been (0, 0, 1), but is inverted!
           And comps.normalv = vector(0, 0, -1) -}
    describe "The hit, when an intersection occurs on the inside" $ do
      let r     = makeRay (T.point 0 0 0) (vector 0 0 1)
          shape = defaultSphere 1
          i     = Intersection 1 shape
          comps = SUT.prepareComputations i r [i]
      it "computation point = point(0, 0, 1)" $ do
        Types.point comps `shouldBe` T.point 0 0 1
      it "computation eyev = vector(0, 0, -1)" $ do
        eyev comps `shouldBe` vector 0 0 (-1)
      it "comps.inside = true" $ do
        inside comps `shouldBe` True
      it "computation normalv = vector(0, 0, -1)" $ do
        normalv comps `shouldBe` vector 0 0 (-1)
    {- Scenario: The hit should offset the point
         Given r ← ray(point(0, 0, -5), vector(0, 0, 1))
           And shape ← sphere() with:
             | transform | translation(0, 0, 1) |
           And i ← intersection(5, shape)
         When comps ← prepare_computations(i, r)
         Then comps.over_point.z < -EPSILON/2
           And comps.point.z > comps.over_point.z -}
    describe "The hit should offset the point" $ do
      let r     = makeRay (T.point 0 0 (-5)) (vector 0 0 1)
          shape = (defaultSphere 1) { Types.transform = translation 0 0 1 }
          i     = Intersection 5 shape
          comps = SUT.prepareComputations i r [i]
          ze    = z (overPoint comps) < (-T.epsilon/2)
          pc    = z (Types.point comps) > z (overPoint comps)
      it "comps.over_point.z < -EPSILON/2" $ do
        ze `shouldBe` True
      it "comps.point.z > comps.over_point.z" $ do
        pc `shouldBe` True
    {- Scenario: The under point is offset below the surface
         Given r ← ray(point(0, 0, -5), vector(0, 0, 1))
           And shape ← glass_sphere() with:
             | transform | translation(0, 0, 1) |
           And i ← intersection(5, shape)
           And xs ← intersections(i)
         When comps ← prepare_computations(i, r, xs)
         Then comps.under_point.z > EPSILON/2
           And comps.point.z < comps.under_point.z -}
    describe "The under point is offset below the surface" $ do
      let r     = makeRay (T.point 0 0 (-5)) (vector 0 0 1)
          shape = (makeGlassSphere 1) { Types.transform = translation 0 0 1 }
          i     = Intersection 5 shape
          xs    = [i]
          comps = SUT.prepareComputations i r xs
      it "comps.under_point.z > EPSILON/2" $ do
        z (underPoint comps) > (T.epsilon/2) `shouldBe` True
      it "comps.point.z < comps.under_point.z" $ do
        z (Types.point comps) < z (underPoint comps) `shouldBe` True
    {- Scenario: Precomputing the reflection vector
         Given shape ← plane()
           And r ← ray(point(0, 1, -1), vector(0, -√2/2, √2/2))
           And i ← intersection(√2, shape)
         When comps ← prepare_computations(i, r)
         Then comps.reflectv = vector(0, √2/2, √2/2) -}
    describe "Precomputing the reflection vector" $ do
      let p = defaultPlane 1
          r = makeRay (T.point 0 1 (-1)) (vector 0 (-(sqrt 2)) (sqrt 2))
          i = Intersection (sqrt 2) p
          comps = SUT.prepareComputations i r [i]
      it "the reflection vector is computed" $ do
        reflectv comps `shouldBe` vector 0 (sqrt 2) (sqrt 2)
    {- Scenario Outline: Finding n1 and n2 at various intersections
         Given A ← glass_sphere() with:
             | transform                 | scaling(2, 2, 2) |
             | material.refractive_index | 1.5              |
           And B ← glass_sphere() with:
             | transform                 | translation(0, 0, -0.25) |
             | material.refractive_index | 2.0                      |
           And C ← glass_sphere() with:
             | transform                 | translation(0, 0, 0.25) |
             | material.refractive_index | 2.5                     |
           And r ← ray(point(0, 0, -4), vector(0, 0, 1))
           And xs ← intersections(2:A, 2.75:B, 3.25:C, 4.75:B, 5.25:C, 6:A)
         When comps ← prepare_computations(xs[<index>], r, xs)
         Then comps.n1 = <n1>
           And comps.n2 = <n2>

         Examples:
           | index | n1  | n2  |
           | 0     | 1.0 | 1.5 |
           | 1     | 1.5 | 2.0 |
           | 2     | 2.0 | 2.5 |
           | 3     | 2.5 | 2.5 |
           | 4     | 2.5 | 1.5 |
           | 5     | 1.5 | 1.0 | -}
    describe "Finding n1 and n2 at various intersections" $ do
      let a  = makeGlassSphere 1
          b  = makeGlassSphere 2
          c  = makeGlassSphere 3
          m  = Types.material a
          a' = a { Types.transform = scaling 2 2 2,
                   Types.material  = m { refractiveIndex = 1.5 }}
          b' = b { Types.transform = scaling 0 0 (-0.25),
                   Types.material  = m { refractiveIndex = 2.0 }}
          c' = c { Types.transform = scaling 0 0 0.25,
                   Types.material  = m { refractiveIndex = 2.5 }}
          r  = makeRay (T.point 0 0 (-4)) (vector 0 0 1)
          xs = [ Intersection 2 a'   , Intersection 2.75 b'
               , Intersection 3.25 c', Intersection 4.75 b'
               , Intersection 5.25 c', Intersection 6 a']
          c0 = SUT.prepareComputations (head xs) r xs
          c1 = SUT.prepareComputations (xs !! 1) r xs
          c2 = SUT.prepareComputations (xs !! 2) r xs
          c3 = SUT.prepareComputations (xs !! 3) r xs
          c4 = SUT.prepareComputations (xs !! 4) r xs
          c5 = SUT.prepareComputations (xs !! 5) r xs
      it "Index 0, n1 1.0, n2 1.5" $ do
        [n1 c0, n2 c0] `shouldBe` [1.0, 1.5]
      it "Index 1, n1 1.5, n2 2.0" $ do
        [n1 c1, n2 c1] `shouldBe` [1.5, 2.0]
      it "Index 2, n1 2.0, n2 2.5" $ do
        [n1 c2, n2 c2] `shouldBe` [2.0, 2.5]
      it "Index 3, n1 2.5, n2 2.5" $ do
        [n1 c3, n2 c3] `shouldBe` [2.5, 2.5]
      it "Index 4, n1 2.5, n2 1.5" $ do
        [n1 c4, n2 c4] `shouldBe` [2.5, 1.5]
      it "Index 5, n1 1.5, n2 1.0" $ do
        [n1 c5, n2 c5] `shouldBe` [1.5, 1.0]
    {- Scenario: The Schlick approximation under total internal reflection
         Given shape ← glass_sphere()
           And r ← ray(point(0, 0, √2/2), vector(0, 1, 0))
           And xs ← intersections(-√2/2:shape, √2/2:shape)
         When comps ← prepare_computations(xs[1], r, xs)
           And reflectance ← schlick(comps)
         Then reflectance = 1.0 -}
    describe "The Schlick approximation under total internal reflection" $ do
      let shape = makeGlassSphere 1
          r     = makeRay (T.point 0 0 (sqrt 2/2)) (vector 0 1 0)
          xs    = [Intersection (-sqrt 2/2) shape, Intersection (sqrt 2/2) shape]
          comps = SUT.prepareComputations (xs !! 1) r xs
          reflectance = SUT.schlick comps
      it "schlick reflectance is 1.0" $ do
        reflectance `shouldBe` 1.0
    {- Scenario: The Schlick approximation with a perpendicular viewing angle
         Given shape ← glass_sphere()
           And r ← ray(point(0, 0, 0), vector(0, 1, 0))
           And xs ← intersections(-1:shape, 1:shape)
         When comps ← prepare_computations(xs[1], r, xs)
           And reflectance ← schlick(comps)
         Then reflectance = 0.04 -}
    describe "The Schlick approximation with a perpendicular viewing angle" $ do
      let shape = makeGlassSphere 1
          r     = makeRay (T.point 0 0 0) (vector 0 1 0)
          xs    = [Intersection (-1) shape, Intersection 1 shape]
          comps = SUT.prepareComputations (xs !! 1) r xs
          reflectance = SUT.schlick comps
      it "schlick reflectance is 0.04" $ do
        -- wrap in vector to get epsilon comparison
        vector reflectance 0 0 `shouldBe` vector 0.04 0 0
      {- Scenario: The Schlick approximation with small angle and n2 > n1
           Given shape ← glass_sphere()
             And r ← ray(point(0, 0.99, -2), vector(0, 0, 1))
             And xs ← intersections(1.8589:shape)
           When comps ← prepare_computations(xs[0], r, xs)
             And reflectance ← schlick(comps)
           Then reflectance = 0.48873 -}
    describe "The Schlick approximation with small angle and n2 > n1" $ do
      let shape = makeGlassSphere 1
          r     = makeRay (T.point 0 0.99 (-2)) (vector 0 0 1)
          xs    = [Intersection 1.8589 shape]
          comps = SUT.prepareComputations (head xs) r xs
          reflectance = SUT.schlick comps
      it "schlick reflectance is 0.48873" $ do
        -- wrap in vector to get epsilon comparison
        vector reflectance 0 0 `shouldBe` vector 0.48873 0 0

intersections :: Spec
intersections =
  describe "Basic Intersections" $ do
    {- Scenario: An intersection encapsulates t and object
         Given s ← sphere()
         When i ← intersection(3.5, s)
         Then i.t = 3.5
           And i.object = s -}
    describe "An intersection encapsulates t and object" $ do
      let s = defaultSphere 1
          i = Intersection 3.5 s
      it "t of intersection is 3.5" $ do
        intersectionT i `shouldBe` 3.5
      it "object is the sphere" $ do
        intersectionObject i `shouldBe` s
    {- Scenario: Aggregating intersections
         Given s ← sphere()
           And i1 ← intersection(1, s)
           And i2 ← intersection(2, s)
         When xs ← intersections(i1, i2)
         Then xs.count = 2
           And xs[0].t = 1
           And xs[1].t = 2 -}
    describe "Aggregating intersections" $ do
      let s = defaultSphere 1
          i1 = Intersection 1 s
          i2 = Intersection 2 s
          -- Feature says to use a type 'Intersections' but it seems to be just a
          -- list for now. Change if needed
          xs = [i1, i2]
      it "the number of intersections is 2" $ do
        length xs `shouldBe` 2
      it "the first intersections t is 1" $ do
        intersectionT (head xs) `shouldBe` 1
      it "the second intersections t is 2" $ do
        intersectionT (last xs) `shouldBe` 2

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
      let s  = defaultSphere 1
          i1 = Intersection 1 s
          i2 = Intersection 2 s
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
      let s  = defaultSphere 1
          i1 = Intersection (-1) s
          i2 = Intersection 1 s
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
      let s  = defaultSphere 1
          i1 = Intersection (-2) s
          i2 = Intersection (-1) s
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
      let s  = defaultSphere 1
          i1 = Intersection 5 s
          i2 = Intersection 7 s
          i3 = Intersection (-3) s
          i4 = Intersection 2 s
          xs = [i1, i2, i3, i4]
          i  = SUT.hit xs
      it "The hit is the first positive intersection" $ do
        i `shouldBe` Just i4
    describe "hit of intersections with and without pepared UV" $ do
      let s  = defaultSphere 1
          i1 = Intersection 5 s
          i2 = Intersection 7 s
          i3 = Intersection (-3) s
          i4 = IntersectionUV 2 s 1 1
          xs = [i1, i2, i3, i4]
          i  = SUT.hit xs
      it "The hit is the first positive intersection" $ do
        i `shouldBe` Just i4
