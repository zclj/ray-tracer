module CameraSpec where

import System.IO.Unsafe (unsafePerformIO)

import Test.Tasty
import Test.Tasty.Hspec as HS

import Matrices
import Camera as SUT

epsilonCompare :: Double -> Double -> Bool
epsilonCompare x y = let epsilon = 0.0001
                     in abs (x - y) < epsilon

cameraTests :: TestTree
cameraTests = testGroup "Camera Tests" [
  testGroup "Specs for"
  [ unsafePerformIO (testSpec "Camera" cameraBasics)]]

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
        transform c `shouldBe` identityV
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
