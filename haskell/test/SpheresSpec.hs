module SpheresSpec where

import System.IO.Unsafe (unsafePerformIO)

import Test.Tasty
import Test.Tasty.Hspec as HS

import Tuples
import Rays
import Matrices
import Transformations
import Materials as M
import Spheres as SUT
import Shapes

spheresTests :: TestTree
spheresTests = testGroup "Spheres Tests" [
  testGroup "Specs for"
  [ unsafePerformIO (testSpec "Spheres" sphereIntersections)
  , unsafePerformIO (testSpec "Spheres" sphereTransformation)
  , unsafePerformIO (testSpec "Spheres" sphereNormals)
  , unsafePerformIO (testSpec "Spheres" sphereMaterials)]]

sphereMaterials :: Spec
sphereMaterials =
  describe "Materials" $ do
    {- Scenario: A sphere has a default material
         Given s ← sphere()
         When m ← s.material
         Then m = material() -}
    describe "A sphere has a default material" $ do
      let s = SUT.makeUnitSphere 1
          m = SUT.sphereMaterial s
      it "is the same as the default material" $ do
        m `shouldBe` M.material
    {- Scenario: A sphere may be assigned a material
         Given s ← sphere()
           And m ← material()
           And m.ambient ← 1
         When s.material ← m
         Then s.material = m -}
    describe "A sphere may be assigned a material" $ do
      let s  = SUT.makeUnitSphere 1
          m  = M.material
          m' = m {ambient = 1}
          s' = s {SUT.sphereMaterial = m'}
      it "it has the new material" $ do
        SUT.sphereMaterial s' `shouldBe` m'
    {- Scenario: A helper for producing a sphere with a glassy material
         Given s ← glass_sphere()
         Then s.transform = identity_matrix
           And s.material.transparency = 1.0
           And s.material.refractive_index = 1.5 -}
    describe "A helper for producing a sphere with a glassy material" $ do
      let s = SUT.makeGlassSphere 1
      it "transform is identity matrix" $ do
        sphereTransform s `shouldBe` identityV
      it "material is transparent (1.0)" $ do
        transparency (sphereMaterial s) `shouldBe` 1.0
      it "refractive indes is 1.5" $ do
        refractiveIndex (sphereMaterial s) `shouldBe` 1.5

sphereNormals :: Spec
sphereNormals =
  describe "Normals" $ do
    {- Scenario: The normal on a sphere at a point on the x axis
         Given s ← sphere()
         When n ← normal_at(s, point(1, 0, 0))
         Then n = vector(1, 0, 0) -}
    describe "The normal on a sphere at a point on the x axis" $ do
      let s = SUT.makeUnitSphere 1
          n = SUT.normalAt s (point 1 0 0)
      it "is the vector(1, 0, 0)" $ do
        n `shouldBe` vector 1 0 0
    {- Scenario: The normal on a sphere at a point on the y axis
         Given s ← sphere()
         When n ← normal_at(s, point(0, 1, 0))
         Then n = vector(0, 1, 0) -}
    describe "The normal on a sphere at a point on the y axis" $ do
      let s = SUT.makeUnitSphere 1
          n = SUT.normalAt s (point 0 1 0)
      it "is the vector(0, 1, 0)" $ do
        n `shouldBe` vector 0 1 0
    {- Scenario: The normal on a sphere at a point on the z axis
         Given s ← sphere()
         When n ← normal_at(s, point(0, 0, 1))
         Then n = vector(0, 0, 1) -}
    describe "The normal on a sphere at a point on the z axis" $ do
      let s = SUT.makeUnitSphere 1
          n = SUT.normalAt s (point 0 0 1)
      it "is the vector(0, 0, 1)" $ do
        n `shouldBe` vector 0 0 1
    {- Scenario: The normal on a sphere at a nonaxial point
         Given s ← sphere()
         When n ← normal_at(s, point(√3/3, √3/3, √3/3))
         Then n = vector(√3/3, √3/3, √3/3) -}
    describe "The normal on a sphere at a nonaxial point" $ do
      let s = SUT.makeUnitSphere 1
          n = SUT.normalAt s (point (sqrt 3 / 3) (sqrt 3 / 3) (sqrt 3 / 3))
      it "is the vector(√3/3, √3/3, √3/3)" $ do
        n `shouldBe` vector (sqrt 3 / 3) (sqrt 3 / 3) (sqrt 3 / 3)
    {- Scenario: The normal is a normalized vector
         Given s ← sphere()
         When n ← normal_at(s, point(√3/3, √3/3, √3/3))
         Then n = normalize(n) -}
    describe "The normal is a normalized vector" $ do
      let s = SUT.makeUnitSphere 1
          n = SUT.normalAt s (point (sqrt 3 / 3) (sqrt 3 / 3) (sqrt 3 / 3))
      it "is the vector(√3/3, √3/3, √3/3)" $ do
        n `shouldBe` norm n
    {- Scenario: Computing the normal on a translated sphere
         Given s ← sphere()
           And set_transform(s, translation(0, 1, 0))
         When n ← normal_at(s, point(0, 1.70711, -0.70711))
         Then n = vector(0, 0.70711, -0.70711) -}
    describe "Computing the normal on a translated sphere" $ do
      let s  = SUT.makeUnitSphere 1
          s' = SUT.setTransform s (translation 0 1 0)
          n  = objectNormalAt s' (point 0 1.70711 (-0.70711))
      it "is the vector(0, 0.70711, -0.70711)" $ do
        n `shouldBe` vector 0 0.70711 (-0.70711)
    {- Scenario: Computing the normal on a transformed sphere
         Given s ← sphere()
           And m ← scaling(1, 0.5, 1) * rotation_z(π/5)
           And set_transform(s, m)
         When n ← normal_at(s, point(0, √2/2, -√2/2))
         Then n = vector(0, 0.97014, -0.24254) -}
    describe "Computing the normal on a transformed sphere" $ do
      let s = SUT.makeUnitSphere 1
          m = scaling 1 0.5 1 `mulV` rotationZ(pi/5)
          s' = SUT.setTransform s m
          n = objectNormalAt s' (point 0 (sqrt 2 / 2) (-(sqrt 2 / 2)))
      it "is the vector(0, 0.97014, -0.24254)" $ do
        n `shouldBe` vector 0 0.97014 (-0.24254)

sphereTransformation :: Spec
sphereTransformation =
  describe "Transformations" $ do
    {- Scenario: A sphere's default transformation
         Given s ← sphere()
         Then s.transform = identity_matrix -}
    describe "A sphere's default transformation" $ do
      let s = SUT.makeUnitSphere 1
      it "is the identity matrix" $ do
        SUT.sphereTransform s `shouldBe` identityV
    {- Scenario: Changing a sphere's transformation
         Given s ← sphere()
           And t ← translation(2, 3, 4)
         When set_transform(s, t)
         Then s.transform = t -}
    describe "Changing a sphere's transformation" $ do
      let s  = SUT.makeUnitSphere 1
          t  = translation 2 3 4
          s' = s { SUT.sphereTransform = t }
      it "result in a new sphere with the new transformation" $ do
        SUT.sphereTransform s' `shouldBe` t

sphereIntersections :: Spec
sphereIntersections =
  describe "Intersections" $ do
    {- Scenario: A ray intersects a sphere at two points
         Given r ← ray(point(0, 0, -5), vector(0, 0, 1))
           And s ← sphere()
         When xs ← intersect(s, r)
         Then xs.count = 2
           And xs[0] = 4.0
           And xs[1] = 6.0 -}
    describe "A ray intersects a sphere at two points" $ do
      let r          = makeRay (point 0 0 (-5)) (vector 0 0 1)
          s          = SUT.makeUnitSphere 1
          xs@(x:y:_) = s `shapeIntersect` r
      it "there are two intersections" $ do
        length xs `shouldBe` 2
      it "intersection one is 4.0" $ do
        intersectionT x `shouldBe` 4.0
      it "intersection two is 6.0" $ do
        intersectionT y `shouldBe` 6.0
    {- Scenario: A ray intersects a sphere at a tangent
         Given r ← ray(point(0, 1, -5), vector(0, 0, 1))
           And s ← sphere()
         When xs ← intersect(s, r)
         Then xs.count = 2
           And xs[0] = 5.0
           And xs[1] = 5.0 -}
    describe "A ray intersects a sphere at a tangent" $ do
      let r          = makeRay (point 0 1 (-5)) (vector 0 0 1)
          s          = SUT.makeUnitSphere 1
          xs@(x:y:_) = s `shapeIntersect` r
      it "there are two intersections" $ do
        length xs `shouldBe` 2
      it "intersection one is 5.0" $ do
        intersectionT x `shouldBe` 5.0
      it "intersection two is 5.0" $ do
        intersectionT y `shouldBe` 5.0
    {- Scenario: A ray misses a sphere
         Given r ← ray(point(0, 2, -5), vector(0, 0, 1))
           And s ← sphere()
         When xs ← intersect(s, r)
         Then xs.count = 0 -}
    describe "A ray misses a sphere" $ do
      let r  = makeRay (point 0 2 (-5)) (vector 0 0 1)
          s  = SUT.makeUnitSphere 1
          xs = s `shapeIntersect` r
      it "there are no intersections" $ do
        length xs `shouldBe` 0
    {- Scenario: A ray originates inside a sphere
         Given r ← ray(point(0, 0, 0), vector(0, 0, 1))
           And s ← sphere()
         When xs ← intersect(s, r)
         Then xs.count = 2
           And xs[0] = -1.0
           And xs[1] = 1.0 -}
    describe "A ray originates inside a sphere" $ do
      let r = makeRay (point 0 0 0) (vector 0 0 1)
          s = SUT.makeUnitSphere 1
          xs@(x:y:_) = s `shapeIntersect` r
      it "there are two intersections" $ do
        length xs `shouldBe` 2
      it "intersection one is -1.0" $ do
        intersectionT x `shouldBe` (-1.0)
      it "intersection two is 1.0" $ do
        intersectionT y `shouldBe` 1.0
    {- Scenario: A sphere is behind a ray
         Given r ← ray(point(0, 0, 5), vector(0, 0, 1))
           And s ← sphere()
         When xs ← intersect(s, r)
         Then xs.count = 2
           And xs[0] = -6.0
           And xs[1] = -4.0 -}
    describe "A sphere is behind a ray" $ do
      let r = makeRay (point 0 0 5) (vector 0 0 1)
          s = SUT.makeUnitSphere 1
          xs@(x:y:_) = s `shapeIntersect` r
      it "there are two intersections" $ do
        length xs `shouldBe` 2
      it "intersection one is -6.0" $ do
        intersectionT x `shouldBe` (-6.0)
      it "intersection two is -4.0" $ do
        intersectionT y `shouldBe` (-4.0)
    {- Scenario: Intersect sets the object on the intersection
         Given r ← ray(point(0, 0, -5), vector(0, 0, 1))
           And s ← sphere()
         When xs ← intersect(s, r)
         Then xs.count = 2
           And xs[0].object = s
           And xs[1].object = s -}
    describe "Intersect sets the object on the intersection" $ do
      let r          = makeRay (point 0 0 (-5)) (vector 0 0 1)
          s          = SUT.makeUnitSphere 1
          xs@(x:y:_) = [s] `intersectShapes` r
      it "there are two intersections" $ do
        length xs `shouldBe` 2
      it "the object of the first intersection is s" $ do
        intersectionObject x `shouldBe` s
      it "the object of the second intersection is s" $ do
        intersectionObject y `shouldBe` s
    {- Scenario: Intersecting a scaled sphere with a ray
         Given r ← ray(point(0, 0, -5), vector(0, 0, 1))
           And s ← sphere()
         When set_transform(s, scaling(2, 2, 2))
           And xs ← intersect(s, r)
         Then xs.count = 2
           And xs[0].t = 3
           And xs[1].t = 7 -}
    describe "Intersecting a scaled sphere with a ray" $ do
      let r          = makeRay (point 0 0 (-5)) (vector 0 0 1)
          s          = SUT.makeUnitSphere 1
          m          = scaling 2 2 2
          s'         = s { SUT.sphereTransform = m }
          xs@(x:y:_) = [s'] `intersectShapes` r
      it "there are two intersections" $ do
        length xs `shouldBe` 2
      it "the object of the first intersection is s" $ do
        intersectionT x `shouldBe` 3
      it "the object of the second intersection is s" $ do
        intersectionT y `shouldBe` 7
    {- Scenario: Intersecting a translated sphere with a ray
         Given r ← ray(point(0, 0, -5), vector(0, 0, 1))
           And s ← sphere()
         When set_transform(s, translation(5, 0, 0))
           And xs ← intersect(s, r)
         Then xs.count = 0 -}
    describe "Intersecting a translated sphere with a ray" $ do
      let r  = makeRay (point 0 0 (-5)) (vector 0 0 1)
          s  = SUT.makeUnitSphere 1
          m  = translation 5 0 0
          s' = s { SUT.sphereTransform = m }
          xs = [s'] `intersectShapes` r
      it "there are two intersections" $ do
        length xs `shouldBe` 0
