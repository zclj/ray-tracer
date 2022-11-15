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
import Rays

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
  , unsafePerformIO (testSpec "Shapes" shapeBounds)
  , unsafePerformIO (testSpec "Shapes" boundingBoxes)]]

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
    {- Scenario: A sphere has a bounding box
         Given shape ← sphere()
         When box ← bounds_of(shape)
         Then box.min = point(-1, -1, -1)
           And box.max = point(1, 1, 1) -}
    describe "A sphere has a bounding box" $ do
      let shape = defaultSphere 1
          box   = bounds shape
      it "box.min = point(-1, -1, -1)" $ do
        boundMin box `shouldBe` Tuples.point (-1) (-1) (-1)
      it "box.max = point(1, 1, 1)" $ do
        boundMax box `shouldBe` Tuples.point 1 1 1
    {- Scenario: A plane has a bounding box
         Given shape ← plane()
         When box ← bounds_of(shape)
         Then box.min = point(-infinity, 0, -infinity)
           And box.max = point(infinity, 0, infinity) -}
    describe "A plane has a bounding box" $ do
      let shape = defaultPlane 1
          box   = bounds shape
      it "box.min = point(-infinity, 0, -infinity)" $ do
        show (boundMin box) `shouldBe` show (Tuples.point (-1/0) 0 (-1/0))
      it "box.max = point(infinity, 0, infinity)" $ do
        show (boundMax box) `shouldBe` show (Tuples.point (1/0) 0 (1/0))
    {- Scenario: A cube has a bounding box
         Given shape ← cube()
         When box ← bounds_of(shape)
         Then box.min = point(-1, -1, -1)
           And box.max = point(1, 1, 1) -}
    describe "A cube has a bounding box" $ do
      let shape = defaultCube 1
          box   = bounds shape
      it "box.min = point(-1, -1, -1)" $ do
        (boundMin box) `shouldBe` (Tuples.point (-1) (-1) (-1))
      it "box.max = point(0, 0, 0)" $ do
        (boundMax box) `shouldBe` (Tuples.point 1 1 1)
    {- Scenario: An unbounded cylinder has a bounding box
         Given shape ← cylinder()
         When box ← bounds_of(shape)
         Then box.min = point(-1, -infinity, -1)
           And box.max = point(1, infinity, 1) -}
    describe "An unbounded cylinder has a bounding box" $ do
      let shape = defaultCylinder 1
          box   = bounds shape
      it "box.min = point(-1, -infinity, -1)" $ do
        show (boundMin box) `shouldBe` show (Tuples.point (-1) (-1/0) (-1))
      it "box.max = point(1, infinity, 1)" $ do
        show (boundMax box) `shouldBe` show (Tuples.point 1 (1/0) 1)
    {- Scenario: A bounded cylinder has a bounding box
         Given shape ← cylinder()
           And shape.minimum ← -5
           And shape.maximum ← 3
         When box ← bounds_of(shape)
         Then box.min = point(-1, -5, -1)
           And box.max = point(1, 3, 1) -}
    describe "A bounded cylinder has a bounding box" $ do
      let shape = (defaultCylinder 1) { minY = (-5), maxY = 3 }
          box   = bounds shape
      it "box.min = point(-1, -5, -1)" $ do
        boundMin box `shouldBe` Tuples.point (-1) (-5) (-1)
      it "box.max = point(1, 3, 1)" $ do
        boundMax box `shouldBe` Tuples.point 1 3 1
    {- Scenario: An unbounded cone has a bounding box
         Given shape ← cone()
         When box ← bounds_of(shape)
         Then box.min = point(-infinity, -infinity, -infinity)
           And box.max = point(infinity, infinity, infinity) -}
    describe "An unbounded cone has a bounding box" $ do
      let shape = defaultCone 1
          box   = bounds shape
      it "box.min = point(-infinity, -infinity, -infinity)" $ do
        show (boundMin box) `shouldBe` show (Tuples.point (-1/0) (-1/0) (-1/0))
      it "box.max = point(infinity, infinity, infinity)" $ do
        show (boundMax box) `shouldBe` show (Tuples.point (1/0) (1/0) (1/0))
    {- Scenario: A bounded cone has a bounding box
         Given shape ← cone()
           And shape.minimum ← -5
           And shape.maximum ← 3
         When box ← bounds_of(shape)
         Then box.min = point(-5, -5, -5)
           And box.max = point(5, 3, 5) -}
    describe "A bounded cone has a bounding box" $ do
      let shape = (defaultCone 1) { minY = (-5), maxY = 3 }
          box   = bounds shape
      it "box.min = point(-5, -5, -5)" $ do
        boundMin box `shouldBe` Tuples.point (-5) (-5) (-5)
      it "box.max = point(5, 3, 5)" $ do
        boundMax box `shouldBe` Tuples.point 5 3 5
    {- Scenario: Querying a shape's bounding box in its parent's space
         Given shape ← sphere()
           And set_transform(shape, translation(1, -3, 5) * scaling(0.5, 2, 4))
         When box ← parent_space_bounds_of(shape)
         Then box.min = point(0.5, -5, 1)
           And box.max = point(1.5, -1, 9) -}
    describe "Querying a shape's bounding box in its parent's space" $ do
      let shape = (defaultSphere 1)
                  { Types.transform =
                    (translation 1 (-3) 5) `Matrices.mul` (scaling 0.5 2 4) }
          box   = parentSpaceBoundsOf shape
      it "box.min = point(0.5, -5, 1)" $ do
        boundMin box `shouldBe` Tuples.point 0.5 (-5) 1
      it "box.max = point(1.5, -1, 9)" $ do
        boundMax box `shouldBe` Tuples.point 1.5 (-1) 9
    {- Scenario Outline: Intersecting a ray with a bounding box at the origin
         Given box ← bounding_box(min=point(-1, -1, -1) max=point(1, 1, 1))
           And direction ← normalize(<direction>)
           And r ← ray(<origin>, direction)
         Then intersects(box, r) is <result>

         Examples:
           | origin            | direction        | result |
           | point(5, 0.5, 0)  | vector(-1, 0, 0) | true   |
           | point(-5, 0.5, 0) | vector(1, 0, 0)  | true   |
           | point(0.5, 5, 0)  | vector(0, -1, 0) | true   |
           | point(0.5, -5, 0) | vector(0, 1, 0)  | true   |
           | point(0.5, 0, 5)  | vector(0, 0, -1) | true   |
           | point(0.5, 0, -5) | vector(0, 0, 1)  | true   |
           | point(0, 0.5, 0)  | vector(0, 0, 1)  | true   |
           | point(-2, 0, 0)   | vector(2, 4, 6)  | false  |
           | point(0, -2, 0)   | vector(6, 2, 4)  | false  |
           | point(0, 0, -2)   | vector(4, 6, 2)  | false  |
           | point(2, 0, 2)    | vector(0, 0, -1) | false  |
           | point(0, 2, 2)    | vector(0, -1, 0) | false  |
           | point(2, 2, 0)    | vector(-1, 0, 0) | false  | -}
    describe "Intersecting a ray with a bounding box at the origin" $ do
      let box = BoundingBox { boundMin = Tuples.point (-1) (-1) (-1)
                            , boundMax = Tuples.point 1 1 1 }
          intersect = (\(o, d) -> intersectBox box (makeRay o (norm d)))
          origins = [ Tuples.point 5 0.5 0
                    , Tuples.point (-5) 0.5 0
                    , Tuples.point 0.5 5 0
                    , Tuples.point 0.5 (-5) 0
                    , Tuples.point 0.5 0 5
                    , Tuples.point 0.5 0 (-5)
                    , Tuples.point 0 0.5 0
                    , Tuples.point (-2) 0 0
                    , Tuples.point 0 (-2) 0
                    , Tuples.point 0 0 (-2)
                    , Tuples.point 2 0 2
                    , Tuples.point 0 2 2
                    , Tuples.point 2 2 0]
          directions = [ Tuples.vector (-1) 0 0
                       , Tuples.vector 1 0 0
                       , Tuples.vector 0 (-1) 0
                       , Tuples.vector 0 1 0
                       , Tuples.vector 0 0 (-1)
                       , Tuples.vector 0 0 1
                       , Tuples.vector 0 0 1
                       , Tuples.vector 2 4 6
                       , Tuples.vector 6 2 4
                       , Tuples.vector 4 6 2
                       , Tuples.vector 0 0 (-1)
                       , Tuples.vector 0 (-1) 0
                       , Tuples.vector (-1) 0 0]
          results = [True, True, True, True, True, True, True, False, False, False, False, False, False]
          is = map intersect (zip origins directions)
      it "intersections are correct" $ do
        is `shouldBe` results
    {- Scenario Outline: Intersecting a ray with a non-cubic bounding box
         Given box ← bounding_box(min=point(5, -2, 0) max=point(11, 4, 7))
           And direction ← normalize(<direction>)
           And r ← ray(<origin>, direction)
         Then intersects(box, r) is <result>

         Examples:
           | origin           | direction        | result |
           | point(15, 1, 2)  | vector(-1, 0, 0) | true   |
           | point(-5, -1, 4) | vector(1, 0, 0)  | true   |
           | point(7, 6, 5)   | vector(0, -1, 0) | true   |
           | point(9, -5, 6)  | vector(0, 1, 0)  | true   |
           | point(8, 2, 12)  | vector(0, 0, -1) | true   |
           | point(6, 0, -5)  | vector(0, 0, 1)  | true   |
           | point(8, 1, 3.5) | vector(0, 0, 1)  | true   |
           | point(9, -1, -8) | vector(2, 4, 6)  | false  |
           | point(8, 3, -4)  | vector(6, 2, 4)  | false  |
           | point(9, -1, -2) | vector(4, 6, 2)  | false  |
           | point(4, 0, 9)   | vector(0, 0, -1) | false  |
           | point(8, 6, -1)  | vector(0, -1, 0) | false  |
           | point(12, 5, 4)  | vector(-1, 0, 0) | false  | -}
    describe "Intersecting a ray with a non-cubic bounding box" $ do
      let box = BoundingBox { boundMin = Tuples.point 5 (-2) 0
                            , boundMax = Tuples.point 11 4 7 }
          intersect = (\(o, d) -> intersectBox box (makeRay o (norm d)))
          origins = [ Tuples.point 15 1 2
                    , Tuples.point (-5) (-1) 4
                    , Tuples.point 7 6 5
                    , Tuples.point 9 (-5) 6
                    , Tuples.point 8 2 12
                    , Tuples.point 6 0 (-5)
                    , Tuples.point 8 1 3.5
                    , Tuples.point 9 (-1) (-8)
                    , Tuples.point 8 3 (-4)
                    , Tuples.point 9 (-1) (-2)
                    , Tuples.point 4 0 9
                    , Tuples.point 8 6 (-1)
                    , Tuples.point 12 5 4]
          directions = [ Tuples.vector (-1) 0 0
                       , Tuples.vector 1 0 0
                       , Tuples.vector 0 (-1) 0
                       , Tuples.vector 0 1 0
                       , Tuples.vector 0 0 (-1)
                       , Tuples.vector 0 0 1
                       , Tuples.vector 0 0 1
                       , Tuples.vector 2 4 6
                       , Tuples.vector 6 2 4
                       , Tuples.vector 4 6 2
                       , Tuples.vector 0 0 (-1)
                       , Tuples.vector 0 (-1) 0
                       , Tuples.vector (-1) 0 0]
          results = [True, True, True, True, True, True, True, False, False, False, False, False, False]
          is = map intersect (zip origins directions)
      it "intersections are correct" $ do
        is `shouldBe` results

boundingBoxes :: Spec
boundingBoxes =
  describe "BoundingBoxes" $ do
    {- Scenario: Adding one bounding box to another
         Given box1 ← bounding_box(min=point(-5, -2, 0) max=point(7, 4, 4))
           And box2 ← bounding_box(min=point(8, -7, -2) max=point(14, 2, 8))
         When box2 is added to box1
         Then box1.min = point(-5, -7, -2)
           And box1.max = point(14, 4, 8) -}
    describe "Adding one bounding box to another" $ do
      let box1 = BoundingBox { boundMin = Tuples.point (-5) (-2) 0
                             , boundMax = Tuples.point 7 4 4 }
          box2 = BoundingBox { boundMin = Tuples.point 8 (-7) (-2)
                             , boundMax = Tuples.point 14 2 8 }
          box' = box1 `addBoxes` box2
      it "box1.min = point(-5, -7, -2)" $ do
        boundMin box' `shouldBe` Tuples.point (-5) (-7) (-2)
      it "box1.max = point(14, 4, 8)" $ do
        boundMax box' `shouldBe` Tuples.point 14 4 8
    {- Scenario Outline: Checking to see if a box contains a given point
         Given box ← bounding_box(min=point(5, -2, 0) max=point(11, 4, 7))
           And p ← <point>
         Then box_contains_point(box, p) is <result>

         Examples:
           | point           | result |
           | point(5, -2, 0) | true   |
           | point(11, 4, 7) | true   |
           | point(8, 1, 3)  | true   |
           | point(3, 0, 3)  | false  |
           | point(8, -4, 3) | false  |
           | point(8, 1, -1) | false  |
           | point(13, 1, 3) | false  |
           | point(8, 5, 3)  | false  |
           | point(8, 1, 8)  | false  | -}
    describe "Checking to see if a box contains a given point" $ do
      let box = BoundingBox { boundMin = Tuples.point 5 (-2) 0
                            , boundMax = Tuples.point 11 4 7 }
          points = [ Tuples.point 5 (-2) 0
                   , Tuples.point 11 4 7
                   , Tuples.point 8 1 3
                   , Tuples.point 3 0 3
                   , Tuples.point 8 (-4) 3
                   , Tuples.point 8 1 (-1)
                   , Tuples.point 13 1 3
                   , Tuples.point 8 5 3
                   , Tuples.point 8 1 8]
          bs = map (boxContainsPoint box) points
      it "points are contained or not contained" $ do
        bs `shouldBe` [True, True, True, False, False, False, False, False, False]
    {- Scenario Outline: Checking to see if a box contains a given box
         Given box ← bounding_box(min=point(5, -2, 0) max=point(11, 4, 7))
           And box2 ← bounding_box(min=<min> max=<max>)
         Then box_contains_box(box, box2) is <result>

         Examples:
           | min              | max             | result |
           | point(5, -2, 0)  | point(11, 4, 7) | true   |
           | point(6, -1, 1)  | point(10, 3, 6) | true   |
           | point(4, -3, -1) | point(10, 3, 6) | false  |
           | point(6, -1, 1)  | point(12, 5, 8) | false  | -}
    describe "Checking to see if a box contains a given box" $ do
      let box  = BoundingBox { boundMin = Tuples.point 5 (-2) 0
                             , boundMax = Tuples.point 11 4 7 }
          points = [ (Tuples.point 5 (-2) 0   , Tuples.point 11 4 7)
                   , (Tuples.point 6 (-1) 1   , Tuples.point 10 3 6)
                   , (Tuples.point 4 (-3) (-1), Tuples.point 10 3 6)
                   , (Tuples.point 6 (-1) 1   , Tuples.point 12 5 8)]
          bs = map (\(min, max) ->
                      boxContainsBox
                      box
                      (BoundingBox { boundMin = min
                                   , boundMax = max }))
               points
      it "points are contained or not contained" $ do
        bs `shouldBe` [True, True, False, False]
    {- Scenario: Transforming a bounding box
         Given box ← bounding_box(min=point(-1, -1, -1) max=point(1, 1, 1))
           And matrix ← rotation_x(π / 4) * rotation_y(π / 4)
         When box2 ← transform(box, matrix)
         Then box2.min = point(-1.4142, -1.7071, -1.7071)
           And box2.max = point(1.4142, 1.7071, 1.7071) -}
    describe "Transforming a bounding box" $ do
      let box  = BoundingBox { boundMin = Tuples.point (-1) (-1) (-1)
                             , boundMax = Tuples.point 1 1 1 }
          matrix = (rotationX (pi/4)) `Matrices.mul` (rotationY (pi/4))
          box2 = transformBox box matrix
      it "box2.min = point(-1.4142, -1.7071, -1.7071)" $ do
        boundMin box2 `shouldBe` Tuples.point (-1.4142) (-1.7071) (-1.7071)
      it "box2.max = point(1.4142, 1.7071, 1.7071)" $ do
        boundMax box2 `shouldBe` Tuples.point 1.4142 1.7071 1.7071
    {- Scenario: Splitting a perfect cube
         Given box ← bounding_box(min=point(-1, -4, -5) max=point(9, 6, 5))
         When (left, right) ← split_bounds(box)
         Then left.min = point(-1, -4, -5)
           And left.max = point(4, 6, 5)
           And right.min = point(4, -4, -5)
           And right.max = point(9, 6, 5) -}
    describe "Splitting a perfect cube" $ do
      let box = BoundingBox { boundMin = Tuples.point (-1) (-4) (-5)
                            , boundMax = Tuples.point 9 6 5 }
          (left, right) = splitBounds box
      it "left.min = point(-1, -4, -5)" $ do
        boundMin left `shouldBe` Tuples.point (-1) (-4) (-5)
      it "left.max = point(4, 6, 5)" $ do
        boundMax left `shouldBe` Tuples.point 4 6 5
      it "right.min = point(4, -4, -5)" $ do
        boundMin right `shouldBe` Tuples.point 4 (-4) (-5)
      it "right.max = point(9, 6, 5)" $ do
        boundMax right `shouldBe` Tuples.point 9 6 5
    {- Scenario: Splitting an x-wide box
         Given box ← bounding_box(min=point(-1, -2, -3) max=point(9, 5.5, 3))
         When (left, right) ← split_bounds(box)
         Then left.min = point(-1, -2, -3)
           And left.max = point(4, 5.5, 3)
           And right.min = point(4, -2, -3)
           And right.max = point(9, 5.5, 3) -}
    describe "Splitting an x-wide box" $ do
      let box = BoundingBox { boundMin = Tuples.point (-1) (-2) (-3)
                            , boundMax = Tuples.point 9 5.5 3 }
          (left, right) = splitBounds box
      it "left.min = point(-1, -2, -3)" $ do
        boundMin left `shouldBe` Tuples.point (-1) (-2) (-3)
      it "left.max = point(4, 5.5, 3)" $ do
        boundMax left `shouldBe` Tuples.point 4 5.5 3
      it "right.min = point(4, -2, -3)" $ do
        boundMin right `shouldBe` Tuples.point 4 (-2) (-3)
      it "right.max = point(9, 5.5, 3)" $ do
        boundMax right `shouldBe` Tuples.point 9 5.5 3

    {- Scenario: Splitting a y-wide box
         Given box ← bounding_box(min=point(-1, -2, -3) max=point(5, 8, 3))
         When (left, right) ← split_bounds(box)
         Then left.min = point(-1, -2, -3)
           And left.max = point(5, 3, 3)
           And right.min = point(-1, 3, -3)
           And right.max = point(5, 8, 3) -}
    describe "Splitting a y-wide box" $ do
      let box = BoundingBox { boundMin = Tuples.point (-1) (-2) (-3)
                            , boundMax = Tuples.point 5 8 3 }
          (left, right) = splitBounds box
      it "left.min = point(-1, -2, -3)" $ do
        boundMin left `shouldBe` Tuples.point (-1) (-2) (-3)
      it "left.max = point(5, 3, 3)" $ do
        boundMax left `shouldBe` Tuples.point 5 3 3
      it "right.min = point(-1, 3, -3)" $ do
        boundMin right `shouldBe` Tuples.point (-1) 3 (-3)
      it "right.max = point(5, 8, 3)" $ do
        boundMax right `shouldBe` Tuples.point 5 8 3

    {- Scenario: Splitting a z-wide box
         Given box ← bounding_box(min=point(-1, -2, -3) max=point(5, 3, 7))
         When (left, right) ← split_bounds(box)
         Then left.min = point(-1, -2, -3)
           And left.max = point(5, 3, 2)
           And right.min = point(-1, -2, 2)
           And right.max = point(5, 3, 7) -}
    describe "Splitting a z-wide box" $ do
      let box = BoundingBox { boundMin = Tuples.point (-1) (-2) (-3)
                            , boundMax = Tuples.point 5 3 7 }
          (left, right) = splitBounds box
      it "left.min = point(-1, -2, -3)" $ do
        boundMin left `shouldBe` Tuples.point (-1) (-2) (-3)
      it "left.max = point(5, 3, 2)" $ do
        boundMax left `shouldBe` Tuples.point 5 3 2
      it "right.min = point(-1, -2, 2)" $ do
        boundMin right `shouldBe` Tuples.point (-1) (-2) 2
      it "right.max = point(5, 3, 7)" $ do
        boundMax right `shouldBe` Tuples.point 5 3 7

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

