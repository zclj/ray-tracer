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
  describe "Make Header" $ do
    let header = SUT.makePPMHeader (Width 100) (Height 100)
    it "creates header with height and width" $ do
      header `shouldBe` ["P3","100 100","255"]
