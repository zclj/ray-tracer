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
import Patterns

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
          shape  = last (SUT.aShapes w)
          mat    = asphereMaterial shape
          mat'   = mat { ambient = 1 }
          shape' = shape { asphereMaterial = mat' }
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
          shape  = APlane 1 (translation 0 (-1) 0) mat
          w'     = w { aShapes = [shape] }
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
          shape  = APlane 1 (translation 0 (-1) 0) mat
          w'     = w { aShapes = [shape] }
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
          lower = APlane 1 (translation 0 (-1) 0) mat
          upper = APlane 2 (translation 0 1 0) mat
          w     = World [lower, upper] light
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
          shape  = APlane 1 (translation 0 (-1) 0) mat
          w'     = w { aShapes = [shape] }
          r      = makeRay (point 0 0 (-3)) (vector 0 (-(sqrt 2)/2) (sqrt 2/2))
          i      = Shapes.Intersection (sqrt 2) shape
          comps  = prepareComputations i r [i]
          color  = SUT.reflectedColor w comps 0
      it "color = color(0, 0, 0)" $ do
        color `shouldBe` Color (Red 0) (Green 0) (Blue 0)
    {- Scenario: The refracted color with an opaque surface
         Given w ← default_world()
           And shape ← the first object in w
           And r ← ray(point(0, 0, -5), vector(0, 0, 1))
           And xs ← intersections(4:shape, 6:shape)
         When comps ← prepare_computations(xs[0], r, xs)
           And c ← refracted_color(w, comps, 5)
         Then c = color(0, 0, 0) -}
    describe "The refracted color with an opaque surface" $ do
      let w     = SUT.defaultWorld
          shape = head (aShapes w)
          r     = makeRay (point 0 0 (-5)) (vector 0 0 1)
          xs    = [Shapes.Intersection 4 shape, Shapes.Intersection 6 shape]
          comps = prepareComputations (xs !! 0) r xs
          color = SUT.refractedColor w comps 5
      it "color = color(0, 0, 0)" $ do
        color `shouldBe` Color (Red 0) (Green 0) (Blue 0)
    {- Scenario: The refracted color at the maximum recursive depth
         Given w ← default_world()
           And shape ← the first object in w
           And shape has:
             | material.transparency     | 1.0 |
             | material.refractive_index | 1.5 |
           And r ← ray(point(0, 0, -5), vector(0, 0, 1))
           And xs ← intersections(4:shape, 6:shape)
         When comps ← prepare_computations(xs[0], r, xs)
           And c ← refracted_color(w, comps, 0)
         Then c = color(0, 0, 0) -}
    describe "The refracted color at the maximum recursive depth" $ do
      let w      = SUT.defaultWorld
          shape  = head (aShapes w)
          m      = (aShapeMaterial shape) { transparency = 1.0, refractiveIndex = 1.5 }
          shape' = shape { asphereMaterial = m }
          r      = makeRay (point 0 0 (-5)) (vector 0 0 1)
          xs     = [Shapes.Intersection 4 shape', Shapes.Intersection 6 shape']
          comps  = prepareComputations (xs !! 0) r xs
          color  = SUT.refractedColor w comps 0
      it "color = color(0, 0, 0)" $ do
        color `shouldBe` Color (Red 0) (Green 0) (Blue 0)
    {- Scenario: The refracted color under total internal reflection
         Given w ← default_world()
           And shape ← the first object in w
           And shape has:
             | material.transparency     | 1.0 |
             | material.refractive_index | 1.5 |
           And r ← ray(point(0, 0, √2/2), vector(0, 1, 0))
           And xs ← intersections(-√2/2:shape, √2/2:shape)
         # NOTE: this time you're inside the sphere, so you need
         # to look at the second intersection, xs[1], not xs[0]
         When comps ← prepare_computations(xs[1], r, xs)
           And c ← refracted_color(w, comps, 5)
         Then c = color(0, 0, 0) -}
    describe "The refracted color under total internal reflection" $ do
      let w      = SUT.defaultWorld
          shape  = head (aShapes w)
          m      = (aShapeMaterial shape) { transparency = 1.0, refractiveIndex = 1.5 }
          shape' = shape { asphereMaterial = m }
          r      = makeRay (point 0 0 (sqrt 2/2)) (vector 0 1 0)
          xs     = [ Shapes.Intersection (-sqrt 2/2) shape'
                   , Shapes.Intersection (sqrt 2/2) shape']
          comps  = prepareComputations (xs !! 1) r xs
          color  = SUT.refractedColor w comps 5
      it "color = color(0, 0, 0)" $ do
        color `shouldBe` Color (Red 0) (Green 0) (Blue 0)
    {- Scenario: The refracted color with a refracted ray
         Given w ← default_world()
           And A ← the first object in w
           And A has:
             | material.ambient | 1.0            |
             | material.pattern | test_pattern() |
           And B ← the second object in w
           And B has:
             | material.transparency     | 1.0 |
             | material.refractive_index | 1.5 |
           And r ← ray(point(0, 0, 0.1), vector(0, 1, 0))
           And xs ← intersections(-0.9899:A, -0.4899:B, 0.4899:B, 0.9899:A)
         When comps ← prepare_computations(xs[2], r, xs)
           And c ← refracted_color(w, comps, 5)
         Then c = color(0, 0.99888, 0.04725) -}
    describe "The refracted color with a refracted ray" $ do
      let w      = SUT.defaultWorld
          a      = head (aShapes w)
          am     = (aShapeMaterial a)
                   { ambient         = 1.0,
                     materialPattern = Just pointPattern}
          a'     = a { asphereMaterial = am }
          b      = last (aShapes w)
          bm     = (aShapeMaterial b)
                   { transparency    = 1.0,
                     refractiveIndex = 1.5 }
          b'     = b {asphereMaterial = bm }
          r      = makeRay (point 0 0 0.1) (vector 0 1 0)
          xs     = [ Shapes.Intersection (-0.9899) a'
                   , Shapes.Intersection (-0.4899) b'
                   , Shapes.Intersection 0.4899 b'
                   , Shapes.Intersection 0.9899 a']
          comps  = prepareComputations (xs !! 2) r xs
          w'     = w { aShapes = [a', b'] }
          color  = SUT.refractedColor w' comps 5
      it "color = color(0, 0.99888, 0.04725)" $ do
        color `shouldBe` Color (Red 0) (Green 0.99888) (Blue 0.04725)

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
          s     = head (SUT.aShapes w)
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
          s     = last (aShapes w')
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
          (ASphere oid or ot om)  = head (aShapes w)
          (ASphere lid lr lt lm)  = last (aShapes w)
          outer' = ASphere oid or ot (om { ambient = 1 })
          inner' = ASphere lid lr lt (lm { ambient = 1 })
          w'     = w { aShapes = [inner', outer'] }
          r      = makeRay (point 0 0 0.75) (vector 0 0 (-1))
          c      = SUT.colorAt w' r 1
      it "c = inner.material.color" $ do
        c `shouldBe` (color lm)
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
                    , aShapes = [ Spheres.toAShape s1, Spheres.toAShape s2]}
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
          xs = intersectShapes (aShapes w) r
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
          w   = SUT.World { aShapes = []
                          , light   = l}
          obj = aShapes w
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
          s1    = ASphere { Shapes.id        = 1
                          , asphereRadius    = 1.0
                          , asphereTransform = identityV
                          , asphereMaterial  = Materials.material
                            { color     = Color (Red 0.8) (Green 1) (Blue 0.6)
                            , diffuse   = 0.7
                            , specular  = 0.2}}
          s2    = ASphere { Shapes.id        = 2
                          , asphereRadius    = 1.0
                          , asphereTransform = scaling 0.5 0.5 0.5
                          , asphereMaterial  = Materials.material}
          w     = defaultWorld
      it "contains Sphere S1" $ do
        head (aShapes w) `shouldBe` s1
      it "contains Sphere S2" $ do
        last (aShapes w) `shouldBe` s2
      it "contains the default light" $ do
        SUT.light w `shouldBe` light
