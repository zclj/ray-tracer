module CanvasSpec where

import System.IO.Unsafe (unsafePerformIO)

import Test.Tasty
import Test.Tasty.Hspec as HS
import Test.Tasty.QuickCheck as QC
import Canvas as SUT
import Tuples

canvasTests :: TestTree
canvasTests = testGroup "Canvas Tests" [
  testGroup "Specs for"
  [ unsafePerformIO (testSpec "Canvas" canvasBasics)
  , unsafePerformIO (testSpec "Canvas" canvasWriting)
  , unsafePerformIO (testSpec "Canvas" canvasPPM)
  --, properties
  ]]

properties :: TestTree
properties = testGroup "Canvas Properties" [qcProps]

qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "width of a canvas is the same as the creation width" $
      \w h -> width (mkCanvas (Width w) (Height h)) == (Width w)]

canvasBasics :: Spec
canvasBasics =
  describe "Basics" $ do
    {-  Scenario: Creating a canvas
          Given c ← canvas(10, 20)
          Then c.width = 10
            And c.height = 20
            And every pixel of c is color(0, 0, 0) -}
    describe "Creating" $ do
      --it "creates a canvas with all black pixels" $ do
      let c = mkCanvas (Width 10) (Height 20)
      it "returns width" $ do
        (width c) `shouldBe` (Width 10)

      it "returns height" $ do
        (height c) `shouldBe` (Height 20)

      it "has every pixel of color (0, 0, 0)" $ do
        (concat c) `shouldSatisfy`
          all (\pixel -> pixel == (Color (Red 0) (Green 0) (Blue 0)))

canvasWriting :: Spec
canvasWriting = 
  describe "Writing pixels" $ do
    {- Scenario: Writing pixels to a canvas
         Given c ← canvas(10, 20)
           And red ← color(1, 0, 0)
         When write_pixel(c, 2, 3, red)
         Then pixel_at(c, 2, 3) = red -}
    let c    = mkCanvas (Width 10) (Height 20)
        red  = Color (Red 1) (Green 0) (Blue 0)
        newC = write c (Width 2) (Height 3) red
    it "writes a color to the canvas" $ do
       pixelAt newC (Width 2) (Height 3) `shouldBe` red

canvasPPM :: Spec
canvasPPM =
  describe "Constructing PPM" $ do
  {- Scenario: Constructing the PPM header
       Given c ← canvas(5, 3)
       When ppm ← canvas_to_ppm(c)
       Then lines 1-3 of ppm are
         """
         P3
         5 3
         255
         """ -}
    describe "header" $ do
      let c   = mkCanvas (Width 5) (Height 3)
          ppm = canvasToPPM c
      it "creates header" $ do
        ppm `shouldBe` "P3\n 5 3\n 255\n"

--pendingWith "Implementation"
