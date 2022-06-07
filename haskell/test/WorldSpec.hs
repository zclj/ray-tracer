module WorldSpec where

import System.IO.Unsafe (unsafePerformIO)

import Test.Tasty
import Test.Tasty.Hspec as HS

import Lights
import Spheres
import Materials
import Matrices
import Transformations
import Intersections
import Tuples
import Rays
import World as SUT

worldTests :: TestTree
worldTests = testGroup "World Tests" [
  testGroup "Specs for"
  [ unsafePerformIO (testSpec "World" worldBasics)
  , unsafePerformIO (testSpec "World" worldIntersections)]]

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
          xs = SUT.intersectWorld w r
          (x1:x2:x3:x4:[]) = xs
      it "contains 4 intersections" $ do
        length xs `shouldBe` 4
      it "xs[0].t = 4" $ do
        t x1 `shouldBe` 4.0
      it "xs[1].t = 4.5" $ do
        t x2 `shouldBe` 4.5
      it "xs[2].t = 5.5" $ do
        t x3 `shouldBe` 5.5
      it "xs[3].t = 6" $ do
        t x4 `shouldBe` 6

worldBasics :: Spec
worldBasics =
  describe "World Basics" $ do
    {- Scenario: Creating a world
         Given w ← world()
         Then w contains no objects
           And w has no light source -}
    describe "Creating a world" $ do
      let l   = pointLight (point 0 0 0) (Color (Red 1) (Green 1) (Blue 1))
          w   = SUT.World { objects = []
                          , light   = l}
          obj = (objects w)
          wl  = (light w)
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
      let light = pointLight (point (-10) (-10) (-10)) (Color (Red 1) (Green 1) (Blue 1))
          s1    = Sphere { Spheres.id        = 1
                         , radius            = 1.0
                         , Spheres.transform = identityV
                         , Spheres.material  = Material
                           { color     = (Color (Red 0.8) (Green 1) (Blue 0.6))
                           , ambient   = 0
                           , diffuse   = 0.7
                           , specular  = 0.2
                           , shininess = 0 }}
          s2    = Sphere { Spheres.id        = 2
                         , radius            = 1.0
                         , Spheres.transform = scaling 0.5 0.5 0.5
                         , Spheres.material  = Material
                           { color     = (Color (Red 0.8) (Green 1) (Blue 0.6))
                           , ambient   = 0
                           , diffuse   = 0.7
                           , specular  = 0.2
                           , shininess = 0 }}
          w     = defaultWorld
      it "contains Sphere S1" $ do
        head (objects w) `shouldBe` s1
      it "contains Sphere S2" $ do
        last (objects w) `shouldBe` s2
      it "contains the default light" $ do
        (SUT.light w) `shouldBe` light
