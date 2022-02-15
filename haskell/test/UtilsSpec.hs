module UtilsSpec where

import System.IO.Unsafe (unsafePerformIO)

import Test.Tasty
import Test.Tasty.Hspec as HS
import Utils as SUT

utilsTests :: TestTree
utilsTests = testGroup "Utils Tests" [
  testGroup "Specs for"
  [ unsafePerformIO (testSpec "Utils" splitLines)]]

splitLines :: Spec
splitLines =
  describe "Split Lines" $ do
    describe "when condition is true" $ do
      it "split the list" $ do
        1 `shouldBe` 1
