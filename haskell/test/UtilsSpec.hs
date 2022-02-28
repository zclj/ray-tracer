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
      it "split the list, including multiple values" $ do
        (SUT.splitList3 xs 5 (map (\x -> LineSegment 1 1 x))) `shouldBe` [[1,2],[3,4]]
      it "split the list with short length, single values" $ do
        (SUT.splitList3 xs 3 (map (\x -> LineSegment 1 1 x))) `shouldBe` [[1],[2],[3],[4]]
