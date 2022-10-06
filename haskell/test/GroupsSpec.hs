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
  , unsafePerformIO (testSpec "Groups" groupIntersections)]]

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
