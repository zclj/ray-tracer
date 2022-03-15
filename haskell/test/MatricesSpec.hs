module MatricesSpec where

import System.IO.Unsafe (unsafePerformIO)

import Test.Tasty
import Test.Tasty.Hspec as HS

import Matrices as SUT

matricesTests :: TestTree
matricesTests = testGroup "Matrices Tests" [
  testGroup "Specs for"
  [ unsafePerformIO (testSpec "Matrices" matricesBasics)
  , unsafePerformIO (testSpec "Matrices" matricesArithmetic)]]

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
    describe "Constructing and inspecting a 4x4 matrix" $ do
      let m = makeMatrix [[1, 2, 3, 4], [5.5, 6.5, 7.5, 8.5],
                          [9, 10, 11, 12], [13.5, 14.5, 15.5, 16.5]]
      it "inspect [0,0] = 1" $ do
        SUT.getAt m (RowIndex 0) (ColumnIndex 0) `shouldBe` 1

      it "inspect [0,3] = 4" $ do
        SUT.getAt m (RowIndex 0) (ColumnIndex 3) `shouldBe` 4

      it "inspect [1,0] = 5.5" $ do
        SUT.getAt m (RowIndex 1) (ColumnIndex 0) `shouldBe` 5.5

      it "inspect [1,2] = 7.5" $ do
        SUT.getAt m (RowIndex 1) (ColumnIndex 2) `shouldBe` 7.5

      it "inspect [2,2] = 11" $ do
        SUT.getAt m (RowIndex 2) (ColumnIndex 2) `shouldBe` 11

      it "inspect [3,0] = 13.5" $ do
        SUT.getAt m (RowIndex 3) (ColumnIndex 0) `shouldBe` 13.5

      it "inspect [3,2] = 15.5" $ do
        SUT.getAt m (RowIndex 3) (ColumnIndex 2) `shouldBe` 15.5

    {-  Scenario: A 2x2 matrix ought to be representable
          Given the following 2x2 matrix M:
            | -3 |  5 |
            |  1 | -2 |
          Then M[0,0] = -3
            And M[0,1] = 5
            And M[1,0] = 1
            And M[1,1] = -2 -}
    describe "Constructing and inspecting a 2x2 matrix" $ do
      let m = makeMatrix [[- 3, 5], [1, -2]]
      it "inspect [0,0] = -3" $ do
        SUT.getAt m (RowIndex 0) (ColumnIndex 0) `shouldBe` (-3)

      it "inspect [0,1] = 5" $ do
        SUT.getAt m (RowIndex 0) (ColumnIndex 1) `shouldBe` 5

      it "inspect [1,0] = 1" $ do
        SUT.getAt m (RowIndex 1) (ColumnIndex 0) `shouldBe` 1

      it "inspect [1,1] = -2" $ do
        SUT.getAt m (RowIndex 1) (ColumnIndex 1) `shouldBe` (-2)

    {- Scenario: A 3x3 matrix ought to be representable
         Given the following 3x3 matrix M:
           | -3 |  5 |  0 |
           |  1 | -2 | -7 |
           |  0 |  1 |  1 |
         Then M[0,0] = -3
           And M[1,1] = -2
           And M[2,2] = 1 -}
    describe "Constructing and inspecting a 3x3 matrix" $ do
      let m = makeMatrix [[- 3, 5, 0], [1, - 2, - 7], [0, 1, 1]]
      it "inspect [0,0] = -3" $ do
        SUT.getAt m (RowIndex 0) (ColumnIndex 0) `shouldBe` (-3)

      it "inspect [1,1] = -2" $ do
        SUT.getAt m (RowIndex 1) (ColumnIndex 1) `shouldBe` (-2)

      it "inspect [2,2] = 1" $ do
        SUT.getAt m (RowIndex 2) (ColumnIndex 2) `shouldBe` 1

    {- Scenario: Matrix equality with identical matrices
         Given the following matrix A:
           | 1 | 2 | 3 | 4 |
           | 5 | 6 | 7 | 8 |
           | 9 | 8 | 7 | 6 |
           | 5 | 4 | 3 | 2 |
         And the following matrix B:
           | 1 | 2 | 3 | 4 |
           | 5 | 6 | 7 | 8 |
           | 9 | 8 | 7 | 6 |
           | 5 | 4 | 3 | 2 |
         Then A = B -}
    describe "Matrix equality with identical matrices" $ do
      let a = makeMatrix [[1, 2, 3, 4], [5, 6, 7, 8],
                          [9, 8, 7, 6], [5, 4, 3, 2]]
          b = makeMatrix [[1, 2, 3, 4], [5, 6, 7, 8],
                          [9, 8, 7, 6], [5, 4, 3, 2]]
      it "A = B" $ do
        a `shouldBe` b

    {- Scenario: Matrix equality with different matrices
         Given the following matrix A:
           | 1 | 2 | 3 | 4 |
           | 5 | 6 | 7 | 8 |
           | 9 | 8 | 7 | 6 |
           | 5 | 4 | 3 | 2 |
         And the following matrix B:
           | 2 | 3 | 4 | 5 |
           | 6 | 7 | 8 | 9 |
           | 8 | 7 | 6 | 5 |
           | 4 | 3 | 2 | 1 |
         Then A != B -}
    describe "Matrix equality with different matrices" $ do
      let a = makeMatrix [[1, 2, 3, 4], [5, 6, 7, 8],
                          [9, 8, 7, 6], [5, 4, 3, 2]]
          b = makeMatrix [[2, 3, 4, 5], [6, 7, 8, 9],
                          [8, 7, 6, 5], [4, 3, 2, 1]]
      it "A != B" $ do
        a /= b

    describe "Matrix equality uses epsilon comparison" $ do
      let a = makeMatrix [[1.000001, 2, 3, 4], [5, 6, 7, 8],
                          [9, 8, 7, 6], [5, 4, 3, 2]]
          b = makeMatrix [[1, 2, 3, 4], [5, 6, 7, 8],
                          [9, 8, 7, 6], [5, 4, 3, 2]]
      it "A == B" $ do
        a `shouldBe` b

matricesArithmetic :: Spec
matricesArithmetic =
  describe "Matrix multiplication" $ do
    {- Scenario: Multiplying two matrices
         Given the following matrix A:
           | 1 | 2 | 3 | 4 |
           | 5 | 6 | 7 | 8 |
           | 9 | 8 | 7 | 6 |
           | 5 | 4 | 3 | 2 |
         And the following matrix B:
           | -2 | 1 | 2 |  3 |
           |  3 | 2 | 1 | -1 |
           |  4 | 3 | 6 |  5 |
           |  1 | 2 | 7 |  8 |
         Then A * B is the following 4x4 matrix:
           | 20|  22 |  50 |  48 |
           | 44|  54 | 114 | 108 |
           | 40|  58 | 110 | 102 |
           | 16|  26 |  46 |  42 | -}
    describe "Multiplying two matrices" $ do
      let a = makeMatrix [[1, 2, 3, 4], [5, 6, 7, 8],
                          [9, 8, 7, 6], [5, 4, 3, 2]]
          b = makeMatrix [[- 2, 1, 2, 3], [3, 2, 1, - 1],
                          [4, 3, 6, 5], [1, 2, 7, 8]]
          c = SUT.mul a b
      it "A * B = C" $ do
        c `shouldBe` a
