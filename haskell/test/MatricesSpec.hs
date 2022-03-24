module MatricesSpec where

import System.IO.Unsafe (unsafePerformIO)

import Test.Tasty
import Test.Tasty.Hspec as HS

import Tuples
import Matrices as SUT

matricesTests :: TestTree
matricesTests = testGroup "Matrices Tests" [
  testGroup "Specs for"
  [ unsafePerformIO (testSpec "Matrices" matricesBasics)
  , unsafePerformIO (testSpec "Matrices" matricesArithmetic)
  , unsafePerformIO (testSpec "Matrices" matrixFunctions)]]

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
        c `shouldBe` makeMatrix [[20, 22, 50, 48], [44, 54, 114, 108],
                                 [40, 58, 110, 102], [16, 26, 46, 42]]
    {- Scenario: A matrix multiplied by a tuple
         Given the following matrix A:
           | 1 | 2 | 3 | 4 |
           | 2 | 4 | 4 | 2 |
           | 8 | 6 | 4 | 1 |
           | 0 | 0 | 0 | 1 |
         And b ← tuple(1, 2, 3, 1)
         Then A * b = tuple(18, 24, 33, 1) -}
    describe "Multiplying a matrix by a tuple" $ do
      let a = makeMatrix [[1, 2, 3, 4], [2, 4, 4, 2],
                          [8, 6, 4, 1], [0, 0, 0, 1]]
          b = Tuple 1 2 3 1
          c = SUT.mulT a b
      it "A * b = C" $ do
        c `shouldBe` Tuple 18 24 33 1
    {- Scenario: Multiplying a matrix by the identity matrix
         Given the following matrix A:
           | 0 | 1 |  2 |  4 |
           | 1 | 2 |  4 |  8 |
           | 2 | 4 |  8 | 16 |
           | 4 | 8 | 16 | 32 |
         Then A * identity_matrix = A -}
    describe "Multiplying a matrix by the identity matrix" $ do
      let a = makeMatrix [[0, 1, 2, 4] , [1, 2, 4, 8],
                          [2, 4, 8, 16], [4, 8, 16, 32]]
          b = SUT.mul a SUT.identity
      it "A * Identity = A" $ do
        a `shouldBe` b
    {- Scenario: Multiplying the identity matrix by a tuple
         Given a ← tuple(1, 2, 3, 4)
         Then identity_matrix * a = a -}
    describe "Multiplying the identity matrix by a tuple" $ do
      let a = Tuple 1 2 3 4
          b = SUT.mulT SUT.identity a
      it "Identity * a = a" $ do
        a `shouldBe` b

matrixFunctions :: Spec
matrixFunctions =
  describe "Matrix Functions" $ do
    {- Scenario: Transposing a matrix
         Given the following matrix A:
           | 0 | 9 | 3 | 0 |
           | 9 | 8 | 0 | 8 |
           | 1 | 8 | 5 | 3 |
           | 0 | 0 | 5 | 8 |
         Then transpose(A) is the following matrix:
           | 0 | 9 | 1 | 0 |
           | 9 | 8 | 8 | 0 |
           | 3 | 0 | 5 | 5 |
           | 0 | 8 | 3 | 8 | -}
    describe "Transpose a matrix" $ do
      let a = makeMatrix [[0, 9, 3, 0], [9, 8, 0, 8],
                          [1, 8, 5, 3], [0, 0, 5, 8]]
          t = SUT.transpose a
          b = makeMatrix [[0, 9, 1, 0], [9, 8, 8, 0],
                          [3, 0, 5, 5], [0, 8, 3, 8]]
      it "Transpose A" $ do
        t `shouldBe` b
    {- Scenario: Transposing the identity matrix
         Given A ← transpose(identity_matrix)
         Then A = identity_matrix -}
    describe "Transposing the identity matrix" $ do
      let t = SUT.transpose SUT.identity
      it "Transpose Identity is Identity" $ do
        t `shouldBe` SUT.identity
    {- Scenario: Calculating the determinant of a 2x2 matrix
         Given the following 2x2 matrix A:
           |  1 | 5 |
           | -3 | 2 |
         Then determinant(A) = 17 -}
    describe "Calculating the determinant of a 2x2 matrix" $ do
      let a = makeMatrix [[1, 5], [- 3, 2]]
          d = SUT.determinant a
      it "Determinant of 2x2" $ do
        d `shouldBe` 17

    {- Scenario: A submatrix of a 3x3 matrix is a 2x2 matrix
       Given the following 3x3 matrix A:
         |  1 | 5 |  0 |
         | -3 | 2 |  7 |
         |  0 | 6 | -3 |
       Then submatrix(A, 0, 2) is the following 2x2 matrix:
         | -3 | 2 |
         |  0 | 6 | -}
    describe "A submatrix of a 3x3 matrix is a 2x2 matrix" $ do
      let a = makeMatrix [[1, 5, 0], [- 3, 2, 7], [0, 6, - 3]]
          s = SUT.submatrix a (RowIndex 0) (ColumnIndex 2)
          b = makeMatrix [[- 3, 2], [0, 6]]
      it "is a 2x2" $ do
        s `shouldBe` b
    {- Scenario: A submatrix of a 4x4 matrix is a 3x3 matrix
         Given the following 4x4 matrix A:
           | -6 |  1 |  1 |  6 |
           | -8 |  5 |  8 |  6 |
           | -1 |  0 |  8 |  2 |
           | -7 |  1 | -1 |  1 |
         Then submatrix(A, 2, 1) is the following 3x3 matrix:
           | -6 |  1 | 6 |
           | -8 |  8 | 6 |
           | -7 | -1 | 1 | -}
    describe "A submatrix of a 4x4 matrix is a 3x3 matrix" $ do
      let a = makeMatrix [[- 6, 1, 1, 6], [- 8, 5, 8, 6],
                          [- 1, 0, 8, 2], [- 7, 1, -1, 1]]
          s = SUT.submatrix a (RowIndex 2) (ColumnIndex 1)
          b = makeMatrix [[- 6, 1, 6], [- 8, 8, 6], [- 7, - 1, 1]]
      it "is a 3x3" $ do
        s `shouldBe` b
    {- Scenario: Calculating a minor of a 3x3 matrix
         Given the following 3x3 matrix A:
           |  3 |  5 |  0 |
           |  2 | -1 | -7 |
           |  6 | -1 |  5 |
           And B ← submatrix(A, 1, 0)
         Then determinant(B) = 25
           And minor(A, 1, 0) = 25 -}
    describe "Calculating a minor of a 3x3 matrix" $ do
      let a = makeMatrix [[3, 5, 0], [2, - 1, - 7], [6, - 1, 5]]
          b = SUT.submatrix a (RowIndex 1) (ColumnIndex 0)
          d = SUT.determinant b
          m = SUT.minor a (RowIndex 1) (ColumnIndex 0)
      it "the minor is the determinant of the submatrix" $ do
        m `shouldBe` d
    {- Scenario: Calculating a cofactor of a 3x3 matrix
         Given the following 3x3 matrix A:
           |  3 |  5 |  0 |
           |  2 | -1 | -7 |
           |  6 | -1 |  5 |
         Then minor(A, 0, 0) = -12
           And cofactor(A, 0, 0) = -12
           And minor(A, 1, 0) = 25
           And cofactor(A, 1, 0) = -25 -}
    describe "Calculating a cofactor of a 3x3 matrix" $ do
      let a = makeMatrix [[3, 5, 0], [2, - 1, - 7], [6, - 1, 5]]
          m1 = SUT.minor a (RowIndex 0) (ColumnIndex 0)
          c1 = SUT.cofactor a (RowIndex 0) (ColumnIndex 0)
          m2 = SUT.minor a (RowIndex 1) (ColumnIndex 0)
          c2 = SUT.cofactor a (RowIndex 1) (ColumnIndex 0)
      it "the minor at [0,0] is -12" $ do
        m1 `shouldBe` - 12
      it "the cofactor at [0,0] is -12" $ do
        c1 `shouldBe` - 12
      it "the minor at [1,0] is 25" $ do
        m2 `shouldBe` 25
      it "the cofactor at [1,0] is -25" $ do
        c2 `shouldBe` - 25
    {- Scenario: Calculating the determinant of a 3x3 matrix
         Given the following 3x3 matrix A:
           |  1 |  2 |  6 |
           | -5 |  8 | -4 |
           |  2 |  6 |  4 |
         Then cofactor(A, 0, 0) = 56
           And cofactor(A, 0, 1) = 12
           And cofactor(A, 0, 2) = -46
           And determinant(A) = -196 -}
    describe "Calculating the determinant of a 3x3 matrix" $ do
      let a  = makeMatrix [[1, 2, 6], [- 5, 8, - 4], [2, 6, 4]]
          c1 = SUT.cofactor a (RowIndex 0) (ColumnIndex 0)
          c2 = SUT.cofactor a (RowIndex 0) (ColumnIndex 1)
          c3 = SUT.cofactor a (RowIndex 0) (ColumnIndex 2)
          d  = SUT.determinant a
      it "the cofactor at [0,0] is 56" $ do
        c1 `shouldBe` 56
      it "the cofactor at [0,1] is 12" $ do
        c2 `shouldBe` 12
      it "the cofactor at [0,2] is -46" $ do
        c3 `shouldBe` - 46
      it "the determinant of A is -196" $ do
        d `shouldBe` - 196
