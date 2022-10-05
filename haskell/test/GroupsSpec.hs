module GroupsSpec where

import System.IO.Unsafe (unsafePerformIO)

import Test.Tasty
import Test.Tasty.Hspec as HS

import Types
import Shapes
import Tuples as T
import Rays
import Matrices

groupTests :: TestTree
groupTests = testGroup "Group Tests" [
  testGroup "Specs for"
  [ unsafePerformIO (testSpec "Groups" groupBasics)]]

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
