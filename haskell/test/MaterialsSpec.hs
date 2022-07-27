module MaterialsSpec where

import System.IO.Unsafe (unsafePerformIO)

import Test.Tasty
import Test.Tasty.Hspec as HS

import Tuples
import Lights
import Materials as SUT
import Patterns
import Spheres

materialTests :: TestTree
materialTests = testGroup "Material Tests" [
  testGroup "Specs for"
  [ unsafePerformIO (testSpec "Materials" materialBasics)
  , unsafePerformIO (testSpec "Materials" materialLighting)]]

materialLighting :: Spec
materialLighting =
  describe "Lighting" $ do
    {- Scenario: Lighting with the eye between the light and the surface
         Given eyev ← vector(0, 0, -1)
           And normalv ← vector(0, 0, -1)
           And light ← point_light(point(0, 0, -10), color(1, 1, 1))
         When result ← lighting(m, light, position, eyev, normalv)
         Then result = color(1.9, 1.9, 1.9) -}
    describe "Lighting with the eye between the light and the surface" $ do
      let position = point 0 0 0
          m        = material
          eyev     = vector 0 0 (-1)
          normalv  = vector 0 0 (-1)
          light    = pointLight (point 0 0 (-10)) (Color (Red 1) (Green 1) (Blue 1))
          s        = makeUnitSphere 1
          result   = lighting m s light position eyev normalv False
      it "result in color(1.9, 1.9, 1.9)" $ do
        result `shouldBe` Color (Red 1.9) (Green 1.9) (Blue 1.9)
    {- Scenario: Lighting with the eye between light and surface, eye offset 45°
         Given eyev ← vector(0, √2/2, -√2/2)
           And normalv ← vector(0, 0, -1)
           And light ← point_light(point(0, 0, -10), color(1, 1, 1))
         When result ← lighting(m, light, position, eyev, normalv)
         Then result = color(1.0, 1.0, 1.0) -}
    describe "Lighting with the eye between light and surface, eye offset 45°" $ do
      let position = point 0 0 0
          m        = material
          eyev     = vector 0 (sqrt 2/2) (sqrt 2/2)
          normalv  = vector 0 0 (-1)
          light    = pointLight (point 0 0 (-10)) (Color (Red 1) (Green 1) (Blue 1))
          s        = makeUnitSphere 1
          result   = lighting m s light position eyev normalv False
      it "result in color(1.0, 1.0, 1.0)" $ do
        result `shouldBe` Color (Red 1.0) (Green 1.0) (Blue 1.0)
    {- Scenario: Lighting with eye opposite surface, light offset 45°
         Given eyev ← vector(0, 0, -1)
           And normalv ← vector(0, 0, -1)
           And light ← point_light(point(0, 10, -10), color(1, 1, 1))
         When result ← lighting(m, light, position, eyev, normalv)
         Then result = color(0.7364, 0.7364, 0.7364) -}
    describe "Lighting with eye opposite surface, light offset 45°" $ do
      let position = point 0 0 0
          m        = material
          eyev     = vector 0 0 (-1)
          normalv  = vector 0 0 (-1)
          light    = pointLight (point 0 10 (-10)) (Color (Red 1) (Green 1) (Blue 1))
          s        = makeUnitSphere 1
          result   = lighting m s light position eyev normalv False
      it "result in color(0.7364, 0.7364, 0.7364)" $ do
        result `shouldBe` Color (Red 0.7364) (Green 0.7364) (Blue 0.7364)
    {- Scenario: Lighting with eye in the path of the reflection vector
         Given eyev ← vector(0, -√2/2, -√2/2)
           And normalv ← vector(0, 0, -1)
           And light ← point_light(point(0, 10, -10), color(1, 1, 1))
         When result ← lighting(m, light, position, eyev, normalv)
         Then result = color(1.6364, 1.6364, 1.6364) -}
    describe "Lighting with eye in the path of the reflection vector" $ do
      let position = point 0 0 0
          m        = material
          eyev     = vector 0 (- (sqrt 2/2)) (- (sqrt 2/2))
          normalv  = vector 0 0 (-1)
          light    = pointLight (point 0 10 (-10)) (Color (Red 1) (Green 1) (Blue 1))
          s        = makeUnitSphere 1
          result   = lighting m s light position eyev normalv False
      it "result in color(1.6364, 1.6364, 1.6364)" $ do
        result `shouldBe` Color (Red 1.6364) (Green 1.6364) (Blue 1.6364)
    {- Scenario: Lighting with the light behind the surface
         Given eyev ← vector(0, 0, -1)
           And normalv ← vector(0, 0, -1)
           And light ← point_light(point(0, 0, 10), color(1, 1, 1))
         When result ← lighting(m, light, position, eyev, normalv)
         Then result = color(0.1, 0.1, 0.1) -}
    describe "Lighting with the light behind the surface" $ do
      let position = point 0 0 0
          m        = material
          eyev     = vector 0 0 (-1)
          normalv  = vector 0 0 (-1)
          light    = pointLight (point 0 0 10) (Color (Red 1) (Green 1) (Blue 1))
          s        = makeUnitSphere 1
          result   = lighting m s light position eyev normalv False
      it "result in color(0.1, 0.1, 0.1)" $ do
        result `shouldBe` Color (Red 0.1) (Green 0.1) (Blue 0.1)
    {- Scenario: Lighting with the surface in shadow
         Given eyev ← vector(0, 0, -1)
           And normalv ← vector(0, 0, -1)
           And light ← point_light(point(0, 0, -10), color(1, 1, 1))
           And in_shadow ← true
         When result ← lighting(m, light, position, eyev, normalv, in_shadow)
         Then result = color(0.1, 0.1, 0.1) -}
    describe "Lighting with the surface in shadow" $ do
      let position = point 0 0 0
          m        = material
          eyev     = vector 0 0 (-1)
          normalv  = vector 0 0 (-1)
          light    = pointLight (point 0 0 (-10)) (Color (Red 1) (Green 1) (Blue 1))
          inShadow = True
          s        = makeUnitSphere 1
          result   = lighting m s light position eyev normalv inShadow
      it "result in color(0.1, 0.1, 0.1)" $ do
        result `shouldBe` Color (Red 0.1) (Green 0.1) (Blue 0.1)
    {- Scenario: Lighting with a pattern applied
         Given m.pattern ← stripe_pattern(color(1, 1, 1), color(0, 0, 0))
           And m.ambient ← 1
           And m.diffuse ← 0
           And m.specular ← 0
           And eyev ← vector(0, 0, -1)
           And normalv ← vector(0, 0, -1)
           And light ← point_light(point(0, 0, -10), color(1, 1, 1))
         When c1 ← lighting(m, light, point(0.9, 0, 0), eyev, normalv, false)
           And c2 ← lighting(m, light, point(1.1, 0, 0), eyev, normalv, false)
         Then c1 = color(1, 1, 1)
           And c2 = color(0, 0, 0) -}
    describe "Lighting with a pattern applied" $ do
      let m        = Material { color     = Color (Red 1) (Green 1) (Blue 1)
                              , ambient   = 1
                              , diffuse   = 0
                              , specular  = 0
                              , shininess = 200
                              , materialPattern =
                                Just (stripePattern
                                       (Color (Red 1) (Green 1) (Blue 1))
                                       (Color (Red 0) (Green 0) (Blue 0)))}
          eyev     = vector 0 0 (-1)
          normalv  = vector 0 0 (-1)
          light    = pointLight (point 0 0 (-10)) (Color (Red 1) (Green 1) (Blue 1))
          inShadow = False
          s        = makeUnitSphere 1
          result1  = lighting m s light (point 0.9 0 0) eyev normalv inShadow
          result2  = lighting m s light (point 1.1 0 0) eyev normalv inShadow
      it "lighting(m, light, point(0.9, 0, 0), eyev, normalv, false) -> color(1, 1, 1)" $ do
        result1 `shouldBe` Color (Red 1) (Green 1) (Blue 1)
      it "lighting(m, light, point(1.1, 0, 0), eyev, normalv, false) -> color(0, 0, 0)" $ do
        result2 `shouldBe` Color (Red 0) (Green 0) (Blue 0)

materialBasics :: Spec
materialBasics =
  describe "Basics" $ do
    {- Scenario: The default material
         Given m ← material()
         Then m.color = color(1, 1, 1)
           And m.ambient = 0.1
           And m.diffuse = 0.9
           And m.specular = 0.9
           And m.shininess = 200.0 -}
    describe "The default material" $ do
      let m = SUT.material
      it "color(1, 1, 1)" $ do
        color m `shouldBe` Color (Red 1) (Green 1) (Blue 1)
      it "ambient = 0.1" $ do
        ambient m `shouldBe` 0.1
      it "diffuse = 0.9" $ do
        diffuse m `shouldBe` 0.9
      it "specular = 0.9" $ do
        specular m `shouldBe` 0.9
      it "shininess = 200.0" $ do
         shininess m `shouldBe` 200.0
