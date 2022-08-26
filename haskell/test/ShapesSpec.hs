module ShapesSpec where

import System.IO.Unsafe (unsafePerformIO)

import Test.Tasty
import Test.Tasty.Hspec as HS

import Matrices
import Materials as M
import Shapes as SUT

data TestShape = TestShape { id :: Int
                           , transform :: VMatrix
                           , material  :: Material}

testShape :: TestShape
testShape = TestShape { ShapesSpec.id = 1, ShapesSpec.transform = identityV, ShapesSpec.material = M.material }

instance IsShape TestShape where
  shapeId        = ShapesSpec.id
  shapeTransform = ShapesSpec.transform
  shapeMaterial  = ShapesSpec.material
  shapeNormalAt  = undefined
  shapeIntersect = undefined

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
        SUT.shapeTransform s `shouldBe` identityV
