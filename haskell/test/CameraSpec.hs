module CameraSpec where

import System.IO.Unsafe (unsafePerformIO)

import Test.Tasty
import Test.Tasty.Hspec as HS

import Matrices
import Rays
import Tuples
import Transformations
import Camera as SUT

epsilonCompare :: Double -> Double -> Bool
epsilonCompare x y = let epsilon = 0.0001
                     in abs (x - y) < epsilon

cameraTests :: TestTree
cameraTests = testGroup "Camera Tests" [
  testGroup "Specs for"
  [ unsafePerformIO (testSpec "Camera" cameraBasics)
  , unsafePerformIO (testSpec "Camera" cameraRays)]]

cameraRays :: Spec
cameraRays =
  describe "Camera Rays" $ do
    {- Scenario: Constructing a ray through the center of the canvas
         Given c ← camera(201, 101, π/2)
         When r ← ray_for_pixel(c, 100, 50)
         Then r.origin = point(0, 0, 0)
           And r.direction = vector(0, 0, -1) -}
    describe "Constructing a ray through the center of the canvas" $ do
      let c = SUT.makeCamera 201 101 (pi/2)
          r = SUT.rayForPixel c 100 50
      it "r.origin = point(0, 0, 0)" $ do
        origin r `shouldBe` point 0 0 0
      it "r.direction = vector(0, 0, -1)" $ do
        direction r `shouldBe` vector 0 0 (-1)
    {- Scenario: Constructing a ray through a corner of the canvas
         Given c ← camera(201, 101, π/2)
         When r ← ray_for_pixel(c, 0, 0)
         Then r.origin = point(0, 0, 0)
           And r.direction = vector(0.66519, 0.33259, -0.66851) -}
    describe "Constructing a ray through a corner of the canvas" $ do
      let c = SUT.makeCamera 201 101 (pi/2)
          r = SUT.rayForPixel c 0 0
      it "r.origin = point(0, 0, 0)" $ do
        origin r `shouldBe` point 0 0 0
      it "r.direction = vector(0.66519, 0.33259, -0.66851)" $ do
        direction r `shouldBe` vector 0.66519 0.33259 (-0.66851)
    {- Scenario: Constructing a ray when the camera is transformed
         Given c ← camera(201, 101, π/2)
         When c.transform ← rotation_y(π/4) * translation(0, -2, 5)
           And r ← ray_for_pixel(c, 100, 50)
         Then r.origin = point(0, 2, -5)
           And r.direction = vector(√2/2, 0, -√2/2) -}
    describe "Constructing a ray when the camera is transformed" $ do
      let c  = SUT.makeCamera 201 101 (pi/2)
          c' = c { SUT.transform = (rotationY (pi/4)) `mulV` (translation 0 (-2) 5) }
          r  = SUT.rayForPixel c' 0 0
      it "r.origin = point(0, 2, -5)" $ do
        origin r `shouldBe` point 0 2 (-5)
      it "r.direction = vector(√2/2, 0, -√2/2)" $ do
        direction r `shouldBe` (vector (sqrt 2/2) 0 (-(sqrt 2/2)))

cameraBasics :: Spec
cameraBasics =
  describe "Camera Basics" $ do
    {- Scenario: Constructing a camera
         Given hsize ← 160
           And vsize ← 120
           And field_of_view ← π/2
         When c ← camera(hsize, vsize, field_of_view)
         Then c.hsize = 160
           And c.vsize = 120
           And c.field_of_view = π/2
           And c.transform = identity_matrix -}
    describe "Constructing a camera" $ do
      let hs  = 160
          vs  = 120
          fov = (pi/2)
          c   = SUT.makeCamera hs vs fov
      it "c.hsize = 160" $ do
        hsize c `shouldBe` 160
      it "c.vsize = 120" $ do
        vsize c `shouldBe` 120
      it "c.field_of_view = π/2" $ do
        fieldOfView c `shouldBe` (pi/2)
      it "c.transform = identity_matrix" $ do
        SUT.transform c `shouldBe` identityV
    {- Scenario: The pixel size for a horizontal canvas
         Given c ← camera(200, 125, π/2)
         Then c.pixel_size = 0.01 -}
    describe "The pixel size for a horizontal canvas" $ do
      let c = SUT.makeCamera 200 125 (pi/2)
      it "c.pixel_size = 0.01" $ do
        True `shouldBe` epsilonCompare (pixelSize c) 0.01
    describe "The pixel size for a vertical canvas" $ do
    {- Scenario: The pixel size for a vertical canvas
         Given c ← camera(125, 200, π/2)
         Then c.pixel_size = 0.01 -}
      let c = SUT.makeCamera 125 200 (pi/2)
      it "c.pixel_size = 0.01" $ do
        True `shouldBe` epsilonCompare (pixelSize c) 0.01
