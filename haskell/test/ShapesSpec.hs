module ShapesSpec where

import System.IO.Unsafe (unsafePerformIO)

import Test.Tasty
import Test.Tasty.Hspec as HS

import Matrices
import Materials as M
import Shapes as SUT
import Types
import Transformations
import Tuples

data TestShape = TestShape { id :: Int
                           , transform :: Matrix
                           , material  :: Material}

testShape :: TestShape
testShape = TestShape
  { ShapesSpec.id = 1, ShapesSpec.transform = identity, ShapesSpec.material = defaultMaterial }

shapesTests :: TestTree
shapesTests = testGroup "Shapes Tests" [
  testGroup "Specs for"
  [ unsafePerformIO (testSpec "Shapes" shapeBasics)]]

shapeBasics :: Spec
shapeBasics =
  describe "Basics" $ do
    {- Scenario: The default transformation
         Given s ← test_shape()
         Then s.transform = identity_matrix -}
    describe "The default transformation" $ do
      let s = testShape
      it "s.transform = identity_matrix" $ do
        ShapesSpec.transform s `shouldBe` identity
    {- Scenario: Converting a point from world to object space
         Given g1 ← group()
           And set_transform(g1, rotation_y(π/2))
           And g2 ← group()
           And set_transform(g2, scaling(2, 2, 2))
           And add_child(g1, g2)
           And s ← sphere()
           And set_transform(s, translation(5, 0, 0))
           And add_child(g2, s)
         When p ← world_to_object(s, point(-2, 0, -10))
         Then p = point(0, 0, -1) -}
    describe "Converting a point from world to object space" $ do
      let g1 = (defaultGroup 1) { Types.transform = rotationY (pi/2) }
          g2 = (defaultGroup 2) { Types.transform = scaling 2 2 2 }
          (g1', g2') = addChild g1 g2
          s = (defaultSphere 3) { Types.transform = translation 5 0 0 }
          (g2'', s') = addChild g2' s
          p = SUT.worldToObject s' (Tuples.point (-2) 0 (-10))
      it "p = point(0, 0, -1)" $ do
        p `shouldBe` Tuples.point 0 0 (-1)
