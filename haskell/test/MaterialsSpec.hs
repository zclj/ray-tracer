module MaterialsSpec where

import System.IO.Unsafe (unsafePerformIO)

import Test.Tasty
import Test.Tasty.Hspec as HS

import Tuples
import Materials as SUT

materialTests :: TestTree
materialTests = testGroup "Material Tests" [
  testGroup "Specs for"
  [ unsafePerformIO (testSpec "Materials" materialBasics)]]

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
