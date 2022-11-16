module GroupsSpec where

import System.IO.Unsafe (unsafePerformIO)

import Test.Tasty
import Test.Tasty.Hspec as HS

import Types
import Shapes
import Tuples as T
import Rays
import Matrices
import Transformations

groupTests :: TestTree
groupTests = testGroup "Group Tests" [
  testGroup "Specs for"
  [ unsafePerformIO (testSpec "Groups" groupBasics)
  , unsafePerformIO (testSpec "Groups" groupIntersections)
  , unsafePerformIO (testSpec "Groups" groupBounds)]]

groupBounds :: Spec
groupBounds =
  describe "Groups" $ do
    {- Scenario: A group has a bounding box that contains its children
         Given s ← sphere()
           And set_transform(s, translation(2, 5, -3) * scaling(2, 2, 2))
           And c ← cylinder()
           And c.minimum ← -2
           And c.maximum ← 2
           And set_transform(c, translation(-4, -1, 4) * scaling(0.5, 1, 0.5))
           And shape ← group()
           And add_child(shape, s)
           And add_child(shape, c)
         When box ← bounds_of(shape)
         Then box.min = point(-4.5, -3, -5)
           And box.max = point(4, 7, 4.5) -}
    describe "A group has a bounding box that contains its children" $ do
      let s      = (defaultSphere 1)
                   { Types.transform =
                     (translation 2 5 (-3)) `Matrices.mul` (scaling 2 2 2) }
          c      = (defaultCylinder 2)
                   { Types.transform =
                     (translation (-4) (-1) 4) `Matrices.mul` (scaling 0.5 1 0.5)
                   , minY = (-2)
                   , maxY = 2 }
          (g, _) = addChildren (defaultGroup 3) [s, c]
          box    = bounds g
      it "box.min = point(-4.5, -3, -5)" $ do
        boundMin box `shouldBe` T.point (-4.5) (-3) (-5)
      it "box.max = point(4, 7, 4.5)" $ do
        boundMax box `shouldBe` T.point 4 7 4.5
    {- Scenario: Partitioning a group's children
         Given s1 ← sphere() with:
             | transform | translation(-2, 0, 0) |
           And s2 ← sphere() with:
             | transform | translation(2, 0, 0) |
           And s3 ← sphere()
           And g ← group() of [s1, s2, s3]
         When (left, right) ← partition_children(g)
         Then g is a group of [s3]
           And left = [s1]
           And right = [s2] -}
    describe "Partitioning a group's children" $ do
      let s1 = (defaultSphere 1) { Types.transform = (translation (-2) 0 0)}
          s2 = (defaultSphere 2) { Types.transform = (translation 2 0 0)}
          s3 = (defaultSphere 3)
          (g, _) = addChildren (defaultGroup 4) [s1, s2, s3]
          (left, right) = partitionChildren g
      it "g is a group of [s3]" $ do
        (children g) `shouldBe` [s3]
      it "left = [s1]" $ do
        left `shouldBe` [s1]
      it "right = [s2]" $ do
        right `shouldBe` [s2]
    {- Scenario: Creating a sub-group from a list of children
         Given s1 ← sphere()
           And s2 ← sphere()
           And g ← group()
         When make_subgroup(g, [s1, s2])
         Then g.count = 1
           And g[0] is a group of [s1, s2] -}
    describe "Creating a sub-group from a list of children" $ do
      let s1 = (defaultSphere 1)
          s2 = (defaultSphere 2)
          g  = makeSubgroup (defaultGroup 3) [s1, s2]
      it "g.count = 1" $ do
        length (children g) `shouldBe` 1
      -- it "g[0] is a group of [s1, s2]" $ do
      --   head (children g) `shouldBe` [s1, s2]

groupIntersections :: Spec
groupIntersections =
  describe "Groups" $ do
    {- Scenario: Intersecting a ray with an empty group
         Given g ← group()
           And r ← ray(point(0, 0, 0), vector(0, 0, 1))
         When xs ← local_intersect(g, r)
         Then xs is empty -}
    describe "Intersecting a ray with an empty group" $ do
      let g  = defaultGroup 1
          r  = makeRay (T.point 0 0 0) (vector 0 0 1)
          xs = localIntersect g r
      it "intersections are empty" $ do
        xs `shouldBe` []
    {- Scenario: Intersecting a ray with a nonempty group
         Given g ← group()
           And s1 ← sphere()
           And s2 ← sphere()
           And set_transform(s2, translation(0, 0, -3))
           And s3 ← sphere()
           And set_transform(s3, translation(5, 0, 0))
           And add_child(g, s1)
           And add_child(g, s2)
           And add_child(g, s3)
         When r ← ray(point(0, 0, -5), vector(0, 0, 1))
           And xs ← local_intersect(g, r)
         Then xs.count = 4
           And xs[0].object = s2
           And xs[1].object = s2
           And xs[2].object = s1
           And xs[3].object = s1 -}
    describe "Intersecting a ray with a nonempty group" $ do
      let g  = defaultGroup 1
          s1 = defaultSphere 2
          s2 = (defaultSphere 3) { Types.transform = translation 0 0 (-3) }
          s3 = (defaultSphere 4) { Types.transform = translation 5 0 0 }
          (g', [s1', s2', s3']) = addChildren g [s1, s2, s3]
          r  = makeRay (T.point 0 0 (-5)) (vector 0 0 1)
          xs = localIntersect g' r
      it "(correct child count)" $ do
        length (children g') `shouldBe` 3
      it "gives 4 intersections" $ do
        length xs `shouldBe` 4
      it "with correct shapes" $ do
        map intersectionObject xs `shouldBe` [s2', s2', s1', s1']
    {- Scenario: Intersecting a transformed group
         Given g ← group()
           And set_transform(g, scaling(2, 2, 2))
           And s ← sphere()
           And set_transform(s, translation(5, 0, 0))
           And add_child(g, s)
         When r ← ray(point(10, 0, -10), vector(0, 0, 1))
           And xs ← intersect(g, r)
         Then xs.count = 2 -}
    describe "Intersecting a transformed group" $ do
      let g = (defaultGroup 1) { Types.transform = scaling 2 2 2 }
          s = (defaultSphere 2) { Types.transform = translation 5 0 0 }
          (g', _) = addChild g s
          r = makeRay (T.point 10 0 (-10)) (vector 0 0 1)
          xs = intersectShapes [g'] r
      it "intersect twice" $ do
        length xs `shouldBe` 2

groupBasics :: Spec
groupBasics =
  describe "Groups" $ do
    {- Scenario: Creating a new group
         Given g ← group()
         Then g.transform = identity_matrix
           And g is empty -}
    describe "Creating a new group" $ do
      let g  = defaultGroup 1
      it "group defaults to identity transform" $ do
        Types.transform g `shouldBe` Matrices.identity
      it "group defaults to empty" $ do
        children g `shouldBe` []
    {- Scenario: Adding a child to a group
         Given g ← group()
           And s ← test_shape()
         When add_child(g, s)
         Then g is not empty
           And g includes s
           And s.parent = g -}
    describe "Adding a child to a group" $ do
      let g = defaultGroup 1
          s = defaultSphere 2
          (g', s') = addChild g s
      it "group includes shape" $ do
        children g' `shouldBe` [s']
      it "the parent of shape is the group" $ do
        parent s' `shouldBe` Just g
