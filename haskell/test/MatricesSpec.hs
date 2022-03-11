module MatricesSpec where

import System.IO.Unsafe (unsafePerformIO)

import Test.Tasty
import Test.Tasty.Hspec as HS

import Matrices as SUT

matricesTests :: TestTree
matricesTests = testGroup "Matrices Tests" [
  testGroup "Specs for"
  [ unsafePerformIO (testSpec "Matrices" matricesBasics)]]

matricesBasics :: Spec
matricesBasics =
  describe "Basics" $ do
    {-  Scenario: Constructing and inspecting a 4x4 matrix
          Given the following 4x4 matrix M:
            |  1   |  2   |  3   |  4   |
            |  5.5 |  6.5 |  7.5 |  8.5 |
            |  9   | 10   | 11   | 12   |
            | 13.5 | 14.5 | 15.5 | 16.5 |
          Then M[0,0] = 1
            And M[0,3] = 4
            And M[1,0] = 5.5
            And M[1,2] = 7.5
            And M[2,2] = 11
            And M[3,0] = 13.5
            And M[3,2] = 15.5 -}
    describe "Creating" $ do
      let m = makeMatrix [[1, 2, 3, 4], [5.5, 6.5, 7.5, 8.5],
                          [9, 10, 11, 12], [13.5, 14.5, 15.5, 16.5]]
      it "4x4 matrix" $ do
        SUT.getAt m (RowIndex 0) (ColumnIndex 0) `shouldBe` 1
