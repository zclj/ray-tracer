module ShapesSpec where

import System.IO.Unsafe (unsafePerformIO)

import Test.Tasty
import Test.Tasty.Hspec as HS

import Matrices
import Materials
import Shapes as SUT

data TestShape = TestShape { transform :: VMatrix
                           , material  :: Material}

testShape :: TestShape
testShape = undefined

instance Shape TestShape where
  transform = ShapesSpec.transform
  material  = ShapesSpec.material

shapesTests :: TestTree
shapesTests = testGroup "Shapes Tests" [
  testGroup "Specs for"
  [unsafePerformIO (testSpec "Shapes" shapeBasics)]]

shapeBasics :: Spec
shapeBasics =
  describe "Basics" $ do
    {- Scenario: The default transformation
         Given s ← test_shape()
         Then s.transform = identity_matrix -}
    describe "The default transformation" $ do
      let s = testShape
      it "s.transform = identity_matrix" $ do
        SUT.transform s `shouldBe` identityV
