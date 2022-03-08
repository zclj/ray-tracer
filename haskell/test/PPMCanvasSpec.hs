module PPMCanvasSpec where

import System.IO.Unsafe (unsafePerformIO)

import Test.Tasty
import Test.Tasty.Hspec as HS
import PPMCanvas as SUT
import Tuples
import Canvas

ppmCanvasTests :: TestTree
ppmCanvasTests = testGroup "PPMCanvas Tests" [
  testGroup "Specs for"
  [ unsafePerformIO (testSpec "PPMCanvas" toPPM)]]

toPPM :: Spec
toPPM =
  describe "String To PPM" $ do

    describe "New is same as old" $ do
      let coloredCanvas = makeCanvasWithColor
                          (Color (Red 1) (Green 0.8) (Blue 0.6)) (Width 10) (Height 2)
          ppm = SUT.canvasToPPMString coloredCanvas
          ppm2 = SUT.canvasToPPMString2 coloredCanvas
      it "creates header with height and width" $ do
        ppm2 `shouldBe` ppm
