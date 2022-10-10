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
    {- Scenario: Converting a normal from object to world space
         Given g1 ← group()
           And set_transform(g1, rotation_y(π/2))
           And g2 ← group()
           And set_transform(g2, scaling(1, 2, 3))
           And add_child(g1, g2)
           And s ← sphere()
           And set_transform(s, translation(5, 0, 0))
           And add_child(g2, s)
         When n ← normal_to_world(s, vector(√3/3, √3/3, √3/3))
         Then n = vector(0.2857, 0.4286, -0.8571) -}
    describe "Converting a normal from object to world space" $ do
      let g1 = (defaultGroup 1) { Types.transform = rotationY (pi/2) }
          g2 = (defaultGroup 2) { Types.transform = scaling 1 2 3 }
          (g1', g2') = addChild g1 g2
          s = (defaultSphere 3) { Types.transform = translation 5 0 0 }
          (g2'', s') = addChild g2' s
          p = SUT.normalToWorld s' (Tuples.vector (sqrt 3/3) (sqrt 3/3) (sqrt 3/3))
      it "n = vector(0.2857, 0.4286, -0.8571)" $ do
        p `shouldBe` Tuples.vector 0.2857 0.4286 (-0.8571)
    {- Scenario: Finding the normal on a child object
         Given g1 ← group()
           And set_transform(g1, rotation_y(π/2))
           And g2 ← group()
           And set_transform(g2, scaling(1, 2, 3))
           And add_child(g1, g2)
           And s ← sphere()
           And set_transform(s, translation(5, 0, 0))
           And add_child(g2, s)
         When n ← normal_at(s, point(1.7321, 1.1547, -5.5774))
         Then n = vector(0.2857, 0.4286, -0.8571) -}
    describe "Finding the normal on a child object" $ do
      let g1 = (defaultGroup 1) { Types.transform = rotationY (pi/2) }
          g2 = (defaultGroup 2) { Types.transform = scaling 1 2 3 }
          (g1', g2') = addChild g1 g2
          s = (defaultSphere 3) { Types.transform = translation 5 0 0 }
          (g2'', s') = addChild g2' s
          n = objectNormalAt s' (Tuples.point 1.7321 1.1547 (-5.5774))
      it "n = vector(0.2857, 0.4286, -0.8571)" $ do
        n `shouldBe` vector 0.2857 0.4286 (-0.8571)

