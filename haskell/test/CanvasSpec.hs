module CanvasSpec where

import System.IO.Unsafe (unsafePerformIO)

import Test.Tasty
import Test.Tasty.Hspec as HS
import Canvas as SUT

canvasTests :: TestTree
canvasTests = testGroup "Canvas Tests" [
  testGroup "Specs for"
  [ unsafePerformIO (testSpec "Canvas" canvasBasics)]]

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
      let c = Canvas (Width 10) (Height 20)
      it "returns width" $ do
        (width c) `shouldBe` (Width 10)

      it "returns height" $ do
        (height c) `shouldBe` (Height 20)


--pendingWith "Implementation"
