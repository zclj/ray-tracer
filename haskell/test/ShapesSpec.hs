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
  [ unsafePerformIO (testSpec "Shapes" shapeBasics)
  , unsafePerformIO (testSpec "Shapes" shapeBounds)]]

-- http://www.raytracerchallenge.com/bonus/bounding-boxes.html
shapeBounds :: Spec
shapeBounds =
  describe "Bounding Boxes" $ do
    {- Scenario: Creating an empty bounding box
         Given box ← bounding_box(empty)
         Then box.min = point(infinity, infinity, infinity)
           And box.max = point(-infinity, -infinity, -infinity) -}
    describe "Creating an empty bounding box" $ do
      let box = defaultBoundingBox
      -- compare with textual representation instead of impl. Ord on Inf
      it "box.min = point(infinity, infinity, infinity)" $ do
        show (boundMin box) `shouldBe` show (Tuples.point (1/0) (1/0) (1/0))
      it "box.max = point(-infinity, -infinity, -infinity)" $ do
        show (boundMax box) `shouldBe` show (Tuples.point (-1/0) (-1/0) (-1/0))
    {- Scenario: Creating a bounding box with volume
         Given box ← bounding_box(min=point(-1, -2, -3) max=point(3, 2, 1))
         Then box.min = point(-1, -2, -3)
           And box.max = point(3, 2, 1) -}
    describe "Creating a bounding box with volume" $ do
      let box = (defaultBoundingBox) { boundMin = (Tuples.point (-1) (-2) (-3))
                                     , boundMax = (Tuples.point 3 2 1) }
      it "box.min = point(-1, -2, -3)" $ do
        boundMin box `shouldBe` (Tuples.point (-1) (-2) (-3))
      it "box.max = point(3, 2, 1)" $ do
        boundMax box `shouldBe` (Tuples.point 3 2 1)
    {- Scenario: Adding points to an empty bounding box
         Given box ← bounding_box(empty)
           And p1 ← point(-5, 2, 0)
           And p2 ← point(7, 0, -3)
         When p1 is added to box
           And p2 is added to box
         Then box.min = point(-5, 0, -3)
           And box.max = point(7, 2, 0) -}
    describe "Adding points to an empty bounding box" $ do
      let box   = defaultBoundingBox
          p1    = Tuples.point (-5) 2 0
          p2    = Tuples.point 7 0 (-3)
          box'  = addBoundingBoxPoint box p1
          box'' = addBoundingBoxPoint box' p2
      it "box.min = point(-5, 0, -3)" $ do
        boundMin box'' `shouldBe` Tuples.point (-5) 0 (-3)
      it "box.max = point(7, 2, 0)" $ do
        boundMax box'' `shouldBe` Tuples.point 7 2 0

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

