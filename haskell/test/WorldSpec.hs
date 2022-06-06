module WorldSpec where

import System.IO.Unsafe (unsafePerformIO)

import Test.Tasty
import Test.Tasty.Hspec as HS

import Lights
import Spheres
import Materials
import Matrices
import Transformations
import Tuples
import World as SUT

worldTests :: TestTree
worldTests = testGroup "World Tests" [
  testGroup "Specs for"
  [ unsafePerformIO (testSpec "World" worldBasics)]]

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
