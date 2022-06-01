module MaterialsSpec where

import System.IO.Unsafe (unsafePerformIO)

import Test.Tasty
import Test.Tasty.Hspec as HS

import Tuples
import Lights
import Materials as SUT

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
          result   = lighting m light position eyev normalv
      it "result in color(1.9, 1.9, 1.9)" $ do
        result `shouldBe` (Color (Red 1.9) (Green 1.9) (Blue 1.9))

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
      it "ambient = 0.1" $ do
        ambient m `shouldBe` 0.1
      it "diffuse = 0.9" $ do
        diffuse m `shouldBe` 0.9
      it "specular = 0.9" $ do
        specular m `shouldBe` 0.9
      it "shininess = 200.0" $ do
         shininess m `shouldBe` 200.0
