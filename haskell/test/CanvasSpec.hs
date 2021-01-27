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
    {-  -}
    describe "A canvas .." $ do
      it "returns x" $ do
        pendingWith "Implementation"


--pendingWith "Implementation"
