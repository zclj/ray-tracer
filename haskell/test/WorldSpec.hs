module WorldSpec where

import System.IO.Unsafe (unsafePerformIO)

import Test.Tasty
import Test.Tasty.Hspec as HS

import Lights
import Spheres
import Planes
import Materials
import Matrices
import Transformations
import Shapes
import Tuples
import Rays
import World as SUT

worldTests :: TestTree
worldTests = testGroup "World Tests" [
  testGroup "Specs for"
  [ unsafePerformIO (testSpec "World" worldBasics)
  , unsafePerformIO (testSpec "World" worldIntersections)
  , unsafePerformIO (testSpec "World" worldShading)
  , unsafePerformIO (testSpec "World" worldReflection)]]

worldReflection :: Spec
worldReflection =
  describe "World Shading" $ do
    {- Scenario: The reflected color for a nonreflective material
         Given w ← default_world()
           And r ← ray(point(0, 0, 0), vector(0, 0, 1))
           And shape ← the second object in w
           And shape.material.ambient ← 1
           And i ← intersection(1, shape)
         When comps ← prepare_computations(i, r)
           And color ← reflected_color(w, comps)
         Then color = color(0, 0, 0) -}
    describe "The reflected color for a nonreflective material" $ do
      let w      = SUT.defaultWorld
          r      = makeRay (point 0 0 0) (vector 0 0 1)
          shape  = last (SUT.sphereObjects w)
          mat    = sphereMaterial shape
          mat'   = mat { ambient = 1 }
          shape' = shape { sphereMaterial = mat' }
          i      = Shapes.Intersection 1 shape'
          comps  = prepareComputations i r [i]
          color  = SUT.reflectedColor w comps 1
      it "color = color(0, 0, 0)" $ do
        color `shouldBe` Color (Red 0) (Green 0) (Blue 0)
    {- Scenario: The reflected color for a reflective material
         Given w ← default_world()
           And shape ← plane() with:
             | material.reflective | 0.5                   |
             | transform           | translation(0, -1, 0) |
           And shape is added to w
           And r ← ray(point(0, 0, -3), vector(0, -√2/2, √2/2))
           And i ← intersection(√2, shape)
         When comps ← prepare_computations(i, r)
           And color ← reflected_color(w, comps)
         Then color = color(0.19032, 0.2379, 0.14274) -}
    describe "The reflected color for a reflective material" $ do
      let w      = SUT.defaultWorld
          mat    = material { reflective = 0.5 }
          shape  = Plane 1 (translation 0 (-1) 0) mat
          w'     = w { planeObjects = [shape] }
          r      = makeRay (point 0 0 (-3)) (vector 0 (-(sqrt 2)/2) (sqrt 2/2))
          i      = Shapes.Intersection (sqrt 2) shape
          comps  = prepareComputations i r [i]
          color  = SUT.reflectedColor w comps 1
      it "color = color(0.19032, 0.2379, 0.14274)" $ do
        color `shouldBe` Color (Red 0.19032) (Green 0.2379) (Blue 0.14274)
    {- Scenario: shade_hit() with a reflective material
         Given w ← default_world()
           And shape ← plane() with:
             | material.reflective | 0.5                   |
             | transform           | translation(0, -1, 0) |
           And shape is added to w
           And r ← ray(point(0, 0, -3), vector(0, -√2/2, √2/2))
           And i ← intersection(√2, shape)
         When comps ← prepare_computations(i, r)
           And color ← shade_hit(w, comps)
         Then color = color(0.87677, 0.92436, 0.82918) -}
    describe "shade_hit() with a reflective material" $ do
      let w      = SUT.defaultWorld
          mat    = material { reflective = 0.5 }
          shape  = Plane 1 (translation 0 (-1) 0) mat
          w'     = w { planeObjects = [shape] }
          r      = makeRay (point 0 0 (-3)) (vector 0 (-(sqrt 2)/2) (sqrt 2/2))
          i      = Shapes.Intersection (sqrt 2) shape
          comps  = prepareComputations i r [i]
          color  = SUT.shadeHit w comps 1
      it "color = color(0.87677, 0.92436, 0.82918)" $ do
        color `shouldBe` Color (Red 0.87677) (Green 0.92436) (Blue 0.82918)
    {- Scenario: color_at() with mutually reflective surfaces
         Given w ← world()
           And w.light ← point_light(point(0, 0, 0), color(1, 1, 1))
           And lower ← plane() with:
             | material.reflective | 1                     |
             | transform           | translation(0, -1, 0) |
           And lower is added to w
           And upper ← plane() with:
             | material.reflective | 1                    |
             | transform           | translation(0, 1, 0) |
           And upper is added to w
           And r ← ray(point(0, 0, 0), vector(0, 1, 0))
         Then color_at(w, r) should terminate successfully -}
    describe "color_at() with mutually reflective surfaces" $ do
      let light = pointLight (point 0 0 0) (Color (Red 1) (Green 1) (Blue 1))
          mat   = material { reflective = 1 }
          lower = Plane 1 (translation 0 (-1) 0) mat
          upper = Plane 2 (translation 0 1 0) mat
          w     = World [] [lower, upper] light
          r     = makeRay (point 0 0 0) (vector 0 1 0)
          c     = colorAt w r 1
      it "terminates" $ do
        c `shouldBe` Color (Red 3.8) (Green 3.8) (Blue 3.8)
    {- Scenario: The reflected color at the maximum recursive depth
         Given w ← default_world()
           And shape ← plane() with:
             | material.reflective | 0.5                   |
             | transform           | translation(0, -1, 0) |
           And shape is added to w
           And r ← ray(point(0, 0, -3), vector(0, -√2/2, √2/2))
           And i ← intersection(√2, shape)
         When comps ← prepare_computations(i, r)
           And color ← reflected_color(w, comps, 0)
         Then color = color(0, 0, 0) -}
    describe "The reflected color at the maximum recursive depth" $ do
      let w      = SUT.defaultWorld
          mat    = material { reflective = 0.5 }
          shape  = Plane 1 (translation 0 (-1) 0) mat
          w'     = w { planeObjects = [shape] }
          r      = makeRay (point 0 0 (-3)) (vector 0 (-(sqrt 2)/2) (sqrt 2/2))
          i      = Shapes.Intersection (sqrt 2) shape
          comps  = prepareComputations i r [i]
          color  = SUT.reflectedColor w comps 0
      it "color = color(0, 0, 0)" $ do
        color `shouldBe` Color (Red 0) (Green 0) (Blue 0)

worldShading :: Spec
worldShading =
  describe "World Shading" $ do
    {- Scenario: Shading an intersection
         Given w ← default_world()
           And r ← ray(point(0, 0, -5), vector(0, 0, 1))
           And shape ← the first object in w
           And i ← intersection(4, shape)
         When comps ← prepare_computations(i, r)
           And c ← shade_hit(w, comps)
         Then c = color(0.38066, 0.47583, 0.2855) -}
    describe "Shading an intersection" $ do
      let w     = SUT.defaultWorld
          r     = makeRay (point 0 0 (-5)) (vector 0 0 1)
          s     = head (SUT.sphereObjects w)
          i     = Shapes.Intersection 4 s
          comps = prepareComputations i r [i]
          c     = SUT.shadeHit w comps 1
      it "shaded color c = color(0.38066, 0.47583, 0.2855)" $ do
        c `shouldBe` Color (Red 0.38066) (Green 0.47583) (Blue 0.2855)
    {- Scenario: Shading an intersection from the inside
         Given w ← default_world()
           And w.light ← point_light(point(0, 0.25, 0), color(1, 1, 1))
           And r ← ray(point(0, 0, 0), vector(0, 0, 1))
           And shape ← the second object in w
           And i ← intersection(0.5, shape)
         When comps ← prepare_computations(i, r)
           And c ← shade_hit(w, comps)
         Then c = color(0.90498, 0.90498, 0.90498) -}
    describe "Shading an intersection from the inside" $ do
      let w     = SUT.defaultWorld
          w'    = w { light = pointLight (point 0 0.25 0)
                              (Color (Red 1) (Green 1) (Blue 1)) }
          r     = makeRay (point 0 0 0) (vector 0 0 1)
          s     = last (sphereObjects w')
          i     = Intersection 0.5 s
          comps = prepareComputations i r [i]
          c     = SUT.shadeHit w' comps 1
      it "shaded color c = color(0.90498, 0.90498, 0.90498)" $ do
        c `shouldBe` Color (Red 0.90498) (Green 0.90498) (Blue 0.90498)
    {- Scenario: The color when a ray misses
         Given w ← default_world()
           And r ← ray(point(0, 0, -5), vector(0, 1, 0))
         When c ← color_at(w, r)
         Then c = color(0, 0, 0) -}
    describe "The color when a ray misses" $ do
      let w = SUT.defaultWorld
          r = makeRay (point 0 0 (-5)) (vector 0 1 0)
          c = SUT.colorAt w r 1
      it "c = color(0, 0, 0)" $ do
        c `shouldBe` Color (Red 0) (Green 0) (Blue 0)
    {- Scenario: The color when a ray hits
         Given w ← default_world()
           And r ← ray(point(0, 0, -5), vector(0, 0, 1))
         When c ← color_at(w, r)
         Then c = color(0.38066, 0.47583, 0.2855) -}
    describe "The color when a ray hits" $ do
      let w = SUT.defaultWorld
          r = makeRay (point 0 0 (-5)) (vector 0 0 1)
          c = SUT.colorAt w r 1
      it "c = color(0.38066, 0.47583, 0.2855)" $ do
        c `shouldBe` Color (Red 0.38066) (Green 0.47583) (Blue 0.2855)
    {- Scenario: The color with an intersection behind the ray
         Given w ← default_world()
           And outer ← the first object in w
           And outer.material.ambient ← 1
           And inner ← the second object in w
           And inner.material.ambient ← 1
           And r ← ray(point(0, 0, 0.75), vector(0, 0, -1))
         When c ← color_at(w, r)
         Then c = inner.material.color -}
    describe "The color with an intersection behind the ray" $ do
      let w      = SUT.defaultWorld
          outer  = head (sphereObjects w)
          inner  = last (sphereObjects w)
          outer' = outer { sphereMaterial = (sphereMaterial outer) { ambient = 1 }}
          inner' = inner { sphereMaterial = (sphereMaterial inner) { ambient = 1 }}
          w'     = w { sphereObjects = [outer', inner'] }
          r      = makeRay (point 0 0 0.75) (vector 0 0 (-1))
          c      = SUT.colorAt w' r 1
      it "c = inner.material.color" $ do
        c `shouldBe` color (sphereMaterial inner)
    {- Scenario: There is no shadow when nothing is collinear with point and light
         Given w ← default_world()
           And p ← point(0, 10, 0)
         Then is_shadowed(w, p) is false -}
    describe "There is no shadow when nothing is collinear with point and light" $ do
      let w = SUT.defaultWorld
          p = point 0 10 0
      it "is_shadowed(w, p) is false" $ do
        isShadowed w p `shouldBe` False
    {- Scenario: The shadow when an object is between the point and the light
         Given w ← default_world()
           And p ← point(10, -10, 10)
         Then is_shadowed(w, p) is true -}
    describe "The shadow when an object is between the point and the light" $ do
      let w = SUT.defaultWorld
          p = point 10 (-10) 10
      it "is_shadowed(w, p) is true" $ do
        isShadowed w p `shouldBe` True
    {- Scenario: There is no shadow when an object is behind the light
         Given w ← default_world()
           And p ← point(-20, 20, -20)
         Then is_shadowed(w, p) is false -}
    describe "There is no shadow when an object is behind the light" $ do
      let w = SUT.defaultWorld
          p = point (-20) 20 (-20)
      it "is_shadowed(w, p) is false" $ do
        isShadowed w p `shouldBe` False
    {- Scenario: There is no shadow when an object is behind the point
         Given w ← default_world()
           And p ← point(-2, 2, -2)
         Then is_shadowed(w, p) is false -}
    describe "There is no shadow when an object is behind the point" $ do
      let w = SUT.defaultWorld
          p = point (-2) 2 (-2)
      it "is_shadowed(w, p) is false" $ do
        isShadowed w p `shouldBe` False
    {- Scenario: shade_hit() is given an intersection in shadow
         Given w ← world()
           And w.light ← point_light(point(0, 0, -10), color(1, 1, 1))
           And s1 ← sphere()
           And s1 is added to w
           And s2 ← sphere() with:
             | transform | translation(0, 0, 10) |
           And s2 is added to w
           And r ← ray(point(0, 0, 5), vector(0, 0, 1))
           And i ← intersection(4, s2)
         When comps ← prepare_computations(i, r)
           And c ← shade_hit(w, comps)
         Then c = color(0.1, 0.1, 0.1) -}
    describe "shade_hit() is given an intersection in shadow" $ do
      let s1 = (makeUnitSphere 1) { sphereTransform = translation 0 0 10 }
          s2 = makeUnitSphere 2
          w = World { light   = pointLight
                                (point 0 0 (-10))
                                (Color (Red 1) (Green 1) (Blue 1))
                    , sphereObjects = [ s1, s2]
                    , planeObjects  = []}
          r = makeRay (point 0 0 5) (vector 0 0 1)
          i = Intersection 4 s2
          comps = prepareComputations i r [i]
          c = SUT.shadeHit w comps 1
      it "c = color(0.1, 0.1, 0.1)" $ do
        c `shouldBe` Color (Red 0.1) (Green 0.1) (Blue 0.1)

worldIntersections :: Spec
worldIntersections =
  describe "World Intersections" $ do
    {- Scenario: Intersect a world with a ray
         Given w ← default_world()
           And r ← ray(point(0, 0, -5), vector(0, 0, 1))
         When xs ← intersect_world(w, r)
         Then xs.count = 4
           And xs[0].t = 4
           And xs[1].t = 4.5
           And xs[2].t = 5.5
           And xs[3].t = 6 -}
    describe "Intersect a world with a ray" $ do
      let w  = SUT.defaultWorld
          r  = makeRay (point 0 0 (-5)) (vector 0 0 1)
          xs = intersectShapes (sphereObjects w) r
          [x1, x2, x3, x4] = xs
      it "contains 4 intersections" $ do
        length xs `shouldBe` 4
      it "xs[0].t = 4" $ do
        intersectionT x1 `shouldBe` 4.0
      it "xs[1].t = 4.5" $ do
        intersectionT x2 `shouldBe` 4.5
      it "xs[2].t = 5.5" $ do
        intersectionT x3 `shouldBe` 5.5
      it "xs[3].t = 6" $ do
        intersectionT x4 `shouldBe` 6

worldBasics :: Spec
worldBasics =
  describe "World Basics" $ do
    {- Scenario: Creating a world
         Given w ← world()
         Then w contains no objects
           And w has no light source -}
    describe "Creating a world" $ do
      let l   = pointLight (point 0 0 0) (Color (Red 1) (Green 1) (Blue 1))
          w   = SUT.World { sphereObjects = []
                          , planeObjects  = []
                          , light   = l}
          obj = sphereObjects w
          wl  = light w
      it "world contains no objects" $ do
        obj `shouldBe` []
      it "world contains light source" $ do
        wl `shouldBe` l
    {- Scenario: The default world
         Given light ← point_light(point(-10, 10, -10), color(1, 1, 1))
           And s1 ← sphere() with:
             | material.color     | (0.8, 1.0, 0.6)        |
             | material.diffuse   | 0.7                    |
             | material.specular  | 0.2                    |
           And s2 ← sphere() with:
             | transform | scaling(0.5, 0.5, 0.5) |
         When w ← default_world()
         Then w.light = light
           And w contains s1
           And w contains s2 -}
    describe "The default world" $ do
      let light = pointLight (point (-10) 10 (-10)) (Color (Red 1) (Green 1) (Blue 1))
          s1    = Sphere { Spheres.id      = 1
                         , radius          = 1.0
                         , sphereTransform = identityV
                         , sphereMaterial  = Materials.material
                           { color     = Color (Red 0.8) (Green 1) (Blue 0.6)
                           , diffuse   = 0.7
                           , specular  = 0.2}}
          s2    = Sphere { Spheres.id      = 2
                         , radius          = 1.0
                         , sphereTransform = scaling 0.5 0.5 0.5
                         , sphereMaterial  = Materials.material}
          w     = defaultWorld
      it "contains Sphere S1" $ do
        head (sphereObjects w) `shouldBe` s1
      it "contains Sphere S2" $ do
        last (sphereObjects w) `shouldBe` s2
      it "contains the default light" $ do
        SUT.light w `shouldBe` light
