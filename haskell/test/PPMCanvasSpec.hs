module PPMCanvasSpec where

import System.IO.Unsafe (unsafePerformIO)

import Test.Tasty
import Test.Tasty.Hspec as HS
import PPMCanvas as SUT

ppmCanvasTests :: TestTree
ppmCanvasTests = testGroup "PPMCanvas Tests" [
  testGroup "Specs for"
  [ unsafePerformIO (testSpec "PPMCanvas" toPPM)]]

toPPM :: Spec
toPPM =
  describe "Make Header" $ do
    it "creates header with height and width" $
      pendingWith "Todo"
