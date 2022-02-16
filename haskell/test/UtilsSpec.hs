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
      let xs = [1, 2, 3, 4]
      it "split the list" $ do
        (SUT.splitList xs 5 (\x -> 1)) `shouldBe` [[1,2],[3,4]]
