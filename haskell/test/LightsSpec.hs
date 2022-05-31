module LightsSpec where

import System.IO.Unsafe (unsafePerformIO)

import Test.Tasty
import Test.Tasty.Hspec as HS

import Tuples
import Lights as SUT

lightTests :: TestTree
lightTests = testGroup "Light Tests" [
  testGroup "Specs for"
  [ unsafePerformIO (testSpec "Lights" lightBasics)]]

lightBasics :: Spec
lightBasics =
  describe "Basics" $ do
    {- Scenario: A point light has a position and intensity
         Given intensity ← color(1, 1, 1)
           And position ← point(0, 0, 0)
         When light ← point_light(position, intensity)
         Then light.position = position
           And light.intensity = intensity -}
    describe "A point light has a position and intensity" $ do
      let initIntensity = Color (Red 1) (Green 1) (Blue 1)
          initPosition  = point 0 0 0
          Light {position = p, intensity = i} = SUT.pointLight initPosition initIntensity
      it "light.position = position" $ do
        p `shouldBe` initPosition
      it "light.intensity = intensity" $ do
        i `shouldBe` initIntensity
