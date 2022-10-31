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
    {- Scenario: Finding the normal on a triangle
         Given t ← triangle(point(0, 1, 0), point(-1, 0, 0), point(1, 0, 0))
         When n1 ← local_normal_at(t, point(0, 0.5, 0))
           And n2 ← local_normal_at(t, point(-0.5, 0.75, 0))
           And n3 ← local_normal_at(t, point(0.5, 0.25, 0))
         Then n1 = t.normal
           And n2 = t.normal
           And n3 = t.normal -}
    describe "Finding the normal on a triangle" $ do
      let t = triangle 1 (T.point 0 1 0) (T.point (-1) 0 0) (T.point 1 0 0)
          n1 = localNormalAt t (T.point 0 0.5 0)
          n2 = localNormalAt t (T.point (-0.5) 0.75 0)
          n3 = localNormalAt t (T.point 0.5 0.25 0)
      it "n1 = t.normal" $ do
        normal t `shouldBe` n1
      it "n2 = t.normal" $ do
        normal t `shouldBe` n2
      it "n3 = t.normal" $ do
        normal t `shouldBe` n3
    {- Scenario: Intersecting a ray parallel to the triangle
         Given t ← triangle(point(0, 1, 0), point(-1, 0, 0), point(1, 0, 0))
           And r ← ray(point(0, -1, -2), vector(0, 1, 0))
         When xs ← local_intersect(t, r)
         Then xs is empty -}
    -- describe "Intersecting a ray parallel to the triangle" $ do
    --   let t = triangle 1 (T.point 0 1 0) (T.point (-1) 0 0) (T.point 1 0 0)
    --       r = makeRay (T.point 0 (-1) (-2)) (T.vector 0 1 0)
    --       xs = localIntersect t r
