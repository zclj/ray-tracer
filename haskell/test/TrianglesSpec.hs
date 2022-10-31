module TrianglesSpec where

import System.IO.Unsafe (unsafePerformIO)

import Test.Tasty
import Test.Tasty.Hspec as HS

import Types
import Shapes
import Tuples as T
import Rays
import Matrices
import Transformations

triangleTests :: TestTree
triangleTests = testGroup "Triangle Tests" [
  testGroup "Specs for"
  [ unsafePerformIO (testSpec "Triangles" triangleBasics)]]

triangleBasics :: Spec
triangleBasics =
  describe "Triangles" $ do
    {- Scenario: Constructing a triangle
         Given p1 ← point(0, 1, 0)
           And p2 ← point(-1, 0, 0)
           And p3 ← point(1, 0, 0)
           And t ← triangle(p1, p2, p3)
         Then t.p1 = p1
           And t.p2 = p2
           And t.p3 = p3
           And t.e1 = vector(-1, -1, 0)
           And t.e2 = vector(1, -1, 0)
           And t.normal = vector(0, 0, -1) -}
    describe "Constructing a triangle" $ do
      let p1 = T.point 0 1 0
          p2 = T.point (-1) 0 0
          p3 = T.point 1 0 0
          t  = triangle 1 p1 p2 p3
      it "t.p1 = p1" $ do
        Types.p1 t `shouldBe` p1
      it "t.p2 = p2" $ do
        Types.p2 t `shouldBe` p2
      it "t.p3 = p3" $ do
        Types.p3 t `shouldBe` p3
      it "t.e1 = vector(-1, -1, 0)" $ do
        Types.e1 t `shouldBe` vector (-1) (-1) 0
      it "t.e2 = vector(1, -1, 0)" $ do
        Types.e2 t `shouldBe` vector 1 (-1) 0
      it "t.normal = vector(0, 0, -1)" $ do
        Types.normal t `shouldBe` vector 0 0 (-1)
