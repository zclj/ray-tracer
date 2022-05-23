module MatricesSpec where

import System.IO.Unsafe (unsafePerformIO)

import Test.Tasty
import Test.Tasty.Hspec as HS
import Test.Tasty.QuickCheck as QC

import Tuples
import Matrices as SUT

matricesTests :: TestTree
matricesTests = testGroup "Matrices Tests" [
  testGroup "Specs for"
  [ unsafePerformIO (testSpec "Matrices" matricesBasics)
  , unsafePerformIO (testSpec "Matrices" matricesArithmetic)
  , unsafePerformIO (testSpec "Matrices" matrixFunctions)
  , properties]]

properties :: TestTree
properties = testGroup "Matrix Properties" [inversedMultiplication,
                                            monoidLaws]

-- https://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html
instance Arbitrary Matrix where
  arbitrary = do
    xs <- vectorOf 4 (vectorOf 4 arbitrary)
    return (SUT.makeMatrix xs)

inversedMultiplication = testGroup "Operations"
  [QC.testProperty "If A * B = C, then C * the inverse of B = A" $
    \a b -> SUT.invertible b ==>
      let c = SUT.mul a b
      in SUT.mul c (SUT.inverse b)  == a]

monoidLaws = testGroup "Monoid Laws"
  [QC.testProperty "Associativity - a <> (b <> c) = (a <> b) <> c" $
    \a b c -> (a::Matrix) <> (b <> c) == (a <> b) <> c,
   QC.testProperty "Left Identity - mempty <> a = a" $
    \a -> mempty (a::Matrix) <> a == a,
   QC.testProperty "Right Identity - a <> mempty = a" $
    \a -> (a::Matrix) <> mempty a == a]

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
          um = makeVMatrix [[1, 2, 3, 4], [5.5, 6.5, 7.5, 8.5],
                            [9, 10, 11, 12], [13.5, 14.5, 15.5, 16.5]]
      it "inspect [0,0] = 1" $ do
        SUT.getAt m (RowIndex 0) (ColumnIndex 0) `shouldBe` 1

      it "inspect [0,0] = 1" $ do
        SUT.getAtV um (RowIndex 0) (ColumnIndex 0) `shouldBe` 1

      it "inspect [0,3] = 4" $ do
        SUT.getAt m (RowIndex 0) (ColumnIndex 3) `shouldBe` 4

      it "inspect [0,3] = 4" $ do
        SUT.getAtV um (RowIndex 0) (ColumnIndex 3) `shouldBe` 4

      it "inspect [1,0] = 5.5" $ do
        SUT.getAt m (RowIndex 1) (ColumnIndex 0) `shouldBe` 5.5

      it "inspect [1,0] = 5.5" $ do
        SUT.getAtV um (RowIndex 1) (ColumnIndex 0) `shouldBe` 5.5

      it "inspect [1,2] = 7.5" $ do
        SUT.getAt m (RowIndex 1) (ColumnIndex 2) `shouldBe` 7.5

      it "inspect [1,2] = 7.5" $ do
        SUT.getAtV um (RowIndex 1) (ColumnIndex 2) `shouldBe` 7.5

      it "inspect [2,2] = 11" $ do
        SUT.getAt m (RowIndex 2) (ColumnIndex 2) `shouldBe` 11

      it "inspect [2,2] = 11" $ do
        SUT.getAtV um (RowIndex 2) (ColumnIndex 2) `shouldBe` 11

      it "inspect [3,0] = 13.5" $ do
        SUT.getAt m (RowIndex 3) (ColumnIndex 0) `shouldBe` 13.5

      it "inspect [3,0] = 13.5" $ do
        SUT.getAtV um (RowIndex 3) (ColumnIndex 0) `shouldBe` 13.5

      it "inspect [3,2] = 15.5" $ do
        SUT.getAt m (RowIndex 3) (ColumnIndex 2) `shouldBe` 15.5

      it "inspect [3,2] = 15.5" $ do
        SUT.getAtV um (RowIndex 3) (ColumnIndex 2) `shouldBe` 15.5        

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
    describe "Multiplying two Vmatrices" $ do
      let a = makeVMatrix [[1, 2, 3, 4], [5, 6, 7, 8],
                           [9, 8, 7, 6], [5, 4, 3, 2]]
          b = makeVMatrix [[- 2, 1, 2, 3], [3, 2, 1, - 1],
                           [4, 3, 6, 5], [1, 2, 7, 8]]
          c = SUT.mulV a b
      it "A * B = C" $ do
        c `shouldBe` makeVMatrix [[20, 22, 50, 48], [44, 54, 114, 108],
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
          au = makeVMatrix [[1, 2, 3, 4], [2, 4, 4, 2],
                            [8, 6, 4, 1], [0, 0, 0, 1]]
          b = Tuple 1 2 3 1
          c = SUT.mulT a b
          cu = SUT.mulTV au b
      it "A * b = C" $ do
        c `shouldBe` Tuple 18 24 33 1
      it "(U) A * b = C" $ do
        cu `shouldBe` Tuple 18 24 33 1
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
          au = makeVMatrix [[0, 1, 2, 4] , [1, 2, 4, 8],
                            [2, 4, 8, 16], [4, 8, 16, 32]]
          bu = SUT.mulV au SUT.identityV
          b = SUT.mul a SUT.identity
      it "A * Identity = A" $ do
        a `shouldBe` b
      it "A * Identity = A" $ do
        au `shouldBe` bu
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
          au = makeVMatrix [[0, 9, 3, 0], [9, 8, 0, 8],
                            [1, 8, 5, 3], [0, 0, 5, 8]]
          t = SUT.transpose a
          tu = SUT.transposeV au
          b = makeMatrix [[0, 9, 1, 0], [9, 8, 8, 0],
                          [3, 0, 5, 5], [0, 8, 3, 8]]
          bu = makeVMatrix [[0, 9, 1, 0], [9, 8, 8, 0],
                            [3, 0, 5, 5], [0, 8, 3, 8]]
      it "Transpose A" $ do
        t `shouldBe` b
      it "Transpose A" $ do
        tu `shouldBe` bu
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
          au = makeUMatrix [[1, 5], [- 3, 2]]
          d = SUT.determinant a
          du = SUT.determinantU au
      it "Determinant of 2x2" $ do
        d `shouldBe` 17
      it "Determinant of 2x2" $ do
        du `shouldBe` 17

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
          au = makeVMatrix [[1, 5, 0], [- 3, 2, 7], [0, 6, - 3]]
          s = SUT.submatrix a (RowIndex 0) (ColumnIndex 2)
          su = SUT.submatrixV au (RowIndex 0) (ColumnIndex 2)
          b = makeMatrix [[- 3, 2], [0, 6]]
          bu = makeVMatrix [[- 3, 2], [0, 6]]
      it "is a 2x2" $ do
        s `shouldBe` b
      it "is a 2x2" $ do
        su `shouldBe` bu
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
          au = makeVMatrix [[- 6, 1, 1, 6], [- 8, 5, 8, 6],
                            [- 1, 0, 8, 2], [- 7, 1, -1, 1]]
          s = SUT.submatrix a (RowIndex 2) (ColumnIndex 1)
          su = SUT.submatrixV au (RowIndex 2) (ColumnIndex 1)
          b = makeMatrix [[- 6, 1, 6], [- 8, 8, 6], [- 7, - 1, 1]]
          bu = makeVMatrix [[- 6, 1, 6], [- 8, 8, 6], [- 7, - 1, 1]]
      it "is a 3x3" $ do
        s `shouldBe` b
      it "is a 3x3" $ do
        su `shouldBe` bu
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
          au = makeVMatrix [[3, 5, 0], [2, - 1, - 7], [6, - 1, 5]]
          b = SUT.submatrix a (RowIndex 1) (ColumnIndex 0)
          bu = SUT.submatrixV au (RowIndex 1) (ColumnIndex 0)
          d = SUT.determinant b
          du = SUT.determinantV bu
          m = SUT.minor a (RowIndex 1) (ColumnIndex 0)
          mu = SUT.minorV au (RowIndex 1) (ColumnIndex 0)
      it "the minor is the determinant of the submatrix" $ do
        m `shouldBe` d
      it "the minor is the determinant of the submatrix" $ do
        mu `shouldBe` du
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
          au = makeVMatrix [[3, 5, 0], [2, - 1, - 7], [6, - 1, 5]]
          m1 = SUT.minor a (RowIndex 0) (ColumnIndex 0)
          m1u = SUT.minorV au (RowIndex 0) (ColumnIndex 0)
          c1 = SUT.cofactor a (RowIndex 0) (ColumnIndex 0)
          c1u = SUT.cofactorV au (RowIndex 0) (ColumnIndex 0)
          m2 = SUT.minor a (RowIndex 1) (ColumnIndex 0)
          m2u = SUT.minorV au (RowIndex 1) (ColumnIndex 0)
          c2 = SUT.cofactor a (RowIndex 1) (ColumnIndex 0)
          c2u = SUT.cofactorV au (RowIndex 1) (ColumnIndex 0)
      it "the minor at [0,0] is -12" $ do
        m1 `shouldBe` - 12
      it "the minor at [0,0] is -12" $ do
        m1u `shouldBe` - 12
      it "the cofactor at [0,0] is -12" $ do
        c1 `shouldBe` - 12
      it "the cofactor at [0,0] is -12" $ do
        c1u `shouldBe` - 12
      it "the minor at [1,0] is 25" $ do
        m2 `shouldBe` 25
      it "the minor at [1,0] is 25" $ do
        m2u `shouldBe` 25
      it "the cofactor at [1,0] is -25" $ do
        c2 `shouldBe` - 25
      it "the cofactor at [1,0] is -25" $ do
        c2u `shouldBe` - 25
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
          au  = makeVMatrix [[1, 2, 6], [- 5, 8, - 4], [2, 6, 4]]
          c1 = SUT.cofactor a (RowIndex 0) (ColumnIndex 0)
          c1u = SUT.cofactorV au (RowIndex 0) (ColumnIndex 0)
          c2 = SUT.cofactor a (RowIndex 0) (ColumnIndex 1)
          c2u = SUT.cofactorV au (RowIndex 0) (ColumnIndex 1)
          c3 = SUT.cofactor a (RowIndex 0) (ColumnIndex 2)
          c3u = SUT.cofactorV au (RowIndex 0) (ColumnIndex 2)
          d  = SUT.determinant a
          du  = SUT.determinantV au
      it "the cofactor at [0,0] is 56" $ do
        c1 `shouldBe` 56
      it "the cofactor at [0,0] is 56" $ do
        c1u `shouldBe` 56
      it "the cofactor at [0,1] is 12" $ do
        c2 `shouldBe` 12
      it "the cofactor at [0,1] is 12" $ do
        c2u `shouldBe` 12
      it "the cofactor at [0,2] is -46" $ do
        c3 `shouldBe` - 46
      it "the cofactor at [0,2] is -46" $ do
        c3u `shouldBe` - 46
      it "the determinant of A is -196" $ do
        d `shouldBe` - 196
      it "the determinant of A is -196" $ do
        du `shouldBe` - 196
    {- Scenario: Calculating the determinant of a 4x4 matrix
         Given the following 4x4 matrix A:
           | -2 | -8 |  3 |  5 |
           | -3 |  1 |  7 |  3 |
           |  1 |  2 | -9 |  6 |
           | -6 |  7 |  7 | -9 |
         Then cofactor(A, 0, 0) = 690
           And cofactor(A, 0, 1) = 447
           And cofactor(A, 0, 2) = 210
           And cofactor(A, 0, 3) = 51
           And determinant(A) = -4071 -}
    describe "Calculating the determinant of a 4x4 matrix" $ do
      let a  = makeMatrix [[- 2, - 8, 3, 5], [- 3, 1, 7, 3],
                           [1, 2, - 9, 6], [- 6, 7, 7, -9]]
          au  = makeVMatrix [[- 2, - 8, 3, 5], [- 3, 1, 7, 3],
                             [1, 2, - 9, 6], [- 6, 7, 7, -9]]
          c1 = SUT.cofactor a (RowIndex 0) (ColumnIndex 0)
          c2 = SUT.cofactor a (RowIndex 0) (ColumnIndex 1)
          c3 = SUT.cofactor a (RowIndex 0) (ColumnIndex 2)
          c4 = SUT.cofactor a (RowIndex 0) (ColumnIndex 3)
          c1u = SUT.cofactorV au (RowIndex 0) (ColumnIndex 0)
          c2u = SUT.cofactorV au (RowIndex 0) (ColumnIndex 1)
          c3u = SUT.cofactorV au (RowIndex 0) (ColumnIndex 2)
          c4u = SUT.cofactorV au (RowIndex 0) (ColumnIndex 3)
          d  = SUT.determinant a
          du  = SUT.determinantV au
      it "the cofactor at [0,0] is 690" $ do
        c1 `shouldBe` 690
      it "the cofactor at [0,0] is 690" $ do
        c1u `shouldBe` 690
      it "the cofactor at [0,1] is 447" $ do
        c2 `shouldBe` 447
      it "the cofactor at [0,1] is 447" $ do
        c2u `shouldBe` 447
      it "the cofactor at [0,2] is 210" $ do
        c3 `shouldBe` 210
      it "the cofactor at [0,2] is 210" $ do
        c3u `shouldBe` 210
      it "the cofactor at [0,3] is 51" $ do
        c4 `shouldBe` 51
      it "the cofactor at [0,3] is 51" $ do
        c4u `shouldBe` 51
      it "the determinant of A is -4071" $ do
        d `shouldBe` - 4071
      it "the determinant of A is -4071" $ do
        du `shouldBe` - 4071
    {- Scenario: Testing an invertible matrix for invertibility
         Given the following 4x4 matrix A:
           |  6 |  4 |  4 |  4 |
           |  5 |  5 |  7 |  6 |
           |  4 | -9 |  3 | -7 |
           |  9 |  1 |  7 | -6 |
         Then determinant(A) = -2120
           And A is invertible -}
    describe "Testing an invertible matrix for invertibility" $ do
      let a = makeMatrix [[6, 4, 4, 4], [5, 5, 7, 6],
                          [4, - 9, 3, - 7], [9, 1, 7, - 6]]
          au = makeVMatrix [[6, 4, 4, 4], [5, 5, 7, 6],
                            [4, - 9, 3, - 7], [9, 1, 7, - 6]]
          d = SUT.determinant a
          du = SUT.determinantV au
      it "the determinant of A is -2120" $ do
        d `shouldBe` - 2120
      it "the determinant of A is -2120" $ do
        du `shouldBe` - 2120
      it "and A is invertible" $ do
        a `shouldSatisfy` SUT.invertible
      it "and A is invertible" $ do
        au `shouldSatisfy` SUT.invertibleV
    {- Scenario: Testing a noninvertible matrix for invertibility
         Given the following 4x4 matrix A:
           | -4 |  2 | -2 | -3 |
           |  9 |  6 |  2 |  6 |
           |  0 | -5 |  1 | -5 |
           |  0 |  0 |  0 |  0 |
         Then determinant(A) = 0
           And A is not invertible -}
    describe "Testing a noninvertible matrix for invertibility" $ do
      let a = makeMatrix [[- 4, 2, - 2, - 3], [9, 6, 2, 6],
                          [0, - 5, 1, - 5], [0, 0, 0, 0]]
          au = makeVMatrix [[- 4, 2, - 2, - 3], [9, 6, 2, 6],
                            [0, - 5, 1, - 5], [0, 0, 0, 0]]
          d = SUT.determinant a
          du = SUT.determinantV au
      it "the determinant of A is 0" $ do
        d `shouldBe` 0
      it "the determinant of A is 0" $ do
        du `shouldBe` 0
      it "and A is NOT invertible" $ do
        a `shouldSatisfy` (not . SUT.invertible)
      it "and A is NOT invertible" $ do
        au `shouldSatisfy` (not . SUT.invertibleV)
    {- Scenario: Calculating the inverse of a matrix
         Given the following 4x4 matrix A:
           | -5 |  2 |  6 | -8 |
           |  1 | -5 |  1 |  8 |
           |  7 |  7 | -6 | -7 |
           |  1 | -3 |  7 |  4 |
           And B ← inverse(A)
         Then determinant(A) = 532
           And cofactor(A, 2, 3) = -160
           And B[3,2] = -160/532
           And cofactor(A, 3, 2) = 105
           And B[2,3] = 105/532
           And B is the following 4x4 matrix:
             |  0.21805 |  0.45113 |  0.24060 | -0.04511 |
             | -0.80827 | -1.45677 | -0.44361 |  0.52068 |
             | -0.07895 | -0.22368 | -0.05263 |  0.19737 |
             | -0.52256 | -0.81391 | -0.30075 |  0.30639 | -}
    describe "Calculating the inverse of a matrix" $ do
      let a = makeMatrix [[- 5, 2, 6, - 8], [1, - 5, 1, 8],
                          [7, 7, - 6, - 7], [1, - 3, 7, 4]]
          au = makeVMatrix [[- 5, 2, 6, - 8], [1, - 5, 1, 8],
                            [7, 7, - 6, - 7], [1, - 3, 7, 4]]
          ba = makeMatrix [[0.21805, 0.45113, 0.24060, - 0.04511],
                           [- 0.80827, - 1.45677, - 0.44361, 0.52068],
                           [- 0.07895, - 0.22368, - 0.05263, 0.19737],
                           [- 0.52256, - 0.81391, - 0.30075, 0.30639]]
          bau = makeVMatrix [[0.21805, 0.45113, 0.24060, - 0.04511],
                             [- 0.80827, - 1.45677, - 0.44361, 0.52068],
                             [- 0.07895, - 0.22368, - 0.05263, 0.19737],
                             [- 0.52256, - 0.81391, - 0.30075, 0.30639]]
          bi = SUT.inverse a
          biu = SUT.inverseV au
          d  = SUT.determinant a
          du  = SUT.determinantV au
          c  = SUT.cofactor a (RowIndex 2) (ColumnIndex 3)
          cu  = SUT.cofactorV au (RowIndex 2) (ColumnIndex 3)
          c2 = SUT.cofactor a (RowIndex 3) (ColumnIndex 2)
          c2u = SUT.cofactorV au (RowIndex 3) (ColumnIndex 2)
      it "the determinant of A is 532" $ do
        d `shouldBe` 532
      it "the determinant of A is 532" $ do
        du `shouldBe` 532
      it "cofactor of A at [2,3] is -160" $ do
        c `shouldBe` - 160
      it "cofactor of A at [2,3] is -160" $ do
        cu `shouldBe` - 160
      it "B [3,2] is -160/532" $ do
        SUT.getAt bi (RowIndex 3) (ColumnIndex 2) `shouldBe` (-160)/532
      it "B [3,2] is -160/532" $ do
        SUT.getAtV biu (RowIndex 3) (ColumnIndex 2) `shouldBe` (-160)/532
      it "cofactor of A at [3,2] is 105" $ do
        c `shouldBe` - 160
      it "cofactor of A at [3,2] is 105" $ do
        cu `shouldBe` - 160
      it "B [2,3] is 105/532" $ do
        SUT.getAt bi (RowIndex 2) (ColumnIndex 3) `shouldBe` 105/532
      it "B [2,3] is 105/532" $ do
        SUT.getAtV biu (RowIndex 2) (ColumnIndex 3) `shouldBe` 105/532
      it "B is inverted" $ do
        bi `shouldBe` ba
      it "B is inverted" $ do
        biu `shouldBe` bau
    {- Scenario: Calculating the inverse of another matrix
         Given the following 4x4 matrix A:
           |  8 | -5 |  9 |  2 |
           |  7 |  5 |  6 |  1 |
           | -6 |  0 |  9 |  6 |
           | -3 |  0 | -9 | -4 |
         Then inverse(A) is the following 4x4 matrix:
           | -0.15385 | -0.15385 | -0.28205 | -0.53846 |
           | -0.07692 |  0.12308 |  0.02564 |  0.03077 |
           |  0.35897 |  0.35897 |  0.43590 |  0.92308 |
           | -0.69231 | -0.69231 | -0.76923 | -1.92308 | -}
    describe "Calculating the inverse of another matrix, A" $ do
      let a = makeMatrix [[8, - 5, 9, 2], [7, 5, 6, 1],
                          [- 6, 0, 9, 6], [- 3, 0, - 9, - 4]]
          i = makeMatrix [[-  0.15385, - 0.15385, - 0.28205, - 0.53846],
                          [- 0.07692,  0.12308,  0.02564,  0.03077],
                          [0.35897, 0.35897, 0.43590, 0.92308],
                          [- 0.69231, - 0.69231, - 0.76923, - 1.92308]]
      it "A is inverted" $ do
        SUT.inverse a `shouldBe` i
    {- Scenario: Calculating the inverse of a third matrix
         Given the following 4x4 matrix A:
           |  9 |  3 |  0 |  9 |
           | -5 | -2 | -6 | -3 |
           | -4 |  9 |  6 |  4 |
           | -7 |  6 |  6 |  2 |
         Then inverse(A) is the following 4x4 matrix:
           | -0.04074 | -0.07778 |  0.14444 | -0.22222 |
           | -0.07778 |  0.03333 |  0.36667 | -0.33333 |
           | -0.02901 | -0.14630 | -0.10926 |  0.12963 |
           |  0.17778 |  0.06667 | -0.26667 |  0.33333 | -}
    describe "Calculating the inverse of a third matrix, A" $ do
      let a = makeMatrix [[9, 3, 0, 9], [- 5, - 2, - 6, - 3],
                          [- 4, 9, 6, 4], [- 7, 6, 6, 2]]
          i = makeMatrix [[- 0.04074, - 0.07778, 0.14444, - 0.22222],
                          [- 0.07778, 0.03333, 0.36667, - 0.33333],
                          [- 0.02901, - 0.14630, - 0.10926, 0.12963],
                          [0.17778, 0.06667, - 0.26667, 0.33333]]
      it "A is inverted" $ do
        SUT.inverse a `shouldBe` i
    {- Scenario: Multiplying a product by its inverse
         Given the following 4x4 matrix A:
             |  3 | -9 |  7 |  3 |
             |  3 | -8 |  2 | -9 |
             | -4 |  4 |  4 |  1 |
             | -6 |  5 | -1 |  1 |
           And the following 4x4 matrix B:
             |  8 |  2 |  2 |  2 |
             |  3 | -1 |  7 |  0 |
             |  7 |  0 |  5 |  4 |
             |  6 | -2 |  0 |  5 |
           And C ← A * B
         Then C * inverse(B) = A -}
    describe "Multiplying a product by its inverse" $ do
      let a = makeMatrix [[3, - 9, 7, 3], [3, - 8, 2, - 9],
                          [- 4, 4, 4, 1], [- 6, 5, - 1, 1]]
          av = makeVMatrix [[3, - 9, 7, 3], [3, - 8, 2, - 9],
                            [- 4, 4, 4, 1], [- 6, 5, - 1, 1]]
          b = makeMatrix [[8, 2, 2, 2], [3, - 1, 7, 0],
                          [7, 0, 5, 4], [6, - 2, 0, 5]]
          bv = makeVMatrix [[8, 2, 2, 2], [3, - 1, 7, 0],
                            [7, 0, 5, 4], [6, - 2, 0, 5]]
          c = SUT.mul a b
          cv = SUT.mulV av bv
      it "will give us the same matrix we started with" $ do
        a `shouldBe` SUT.mul c (SUT.inverse b)
      it "will give us the same matrix we started with" $ do
        av `shouldBe` SUT.mulV cv (SUT.inverseV bv)
