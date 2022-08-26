module Matrices
  ( Matrix
  , makeMatrix
  , getAt
  , RowIndex (..)
  , ColumnIndex (..)
  , mulT
  , mul
  , identity
  , transpose
  , determinant
  , submatrix
  , minor
  , cofactor
  , invertible
  , inverse
  ) where

import qualified Tuples as T
import Vector

----
-- Vector based Matrix

makeMatrix4x4 [ [a11, a12, a13, a14]
              , [a21, a22, a23, a24]
              , [a31, a32, a33, a34]
              , [a41, a42, a43, a44]]
  = Matrix4x4 (Vector4D a11 a12 a13 a14)
               (Vector4D a21 a22 a23 a24)
               (Vector4D a31 a32 a33 a34)
               (Vector4D a41 a42 a43 a44)

makeMatrix3x3 [ [a11, a12, a13]
              , [a21, a22, a23]
              , [a31, a32, a33]]
  = Matrix3x3 (Vector3D a11 a12 a13)
               (Vector3D a21 a22 a23)
               (Vector3D a31 a32 a33)

makeMatrix2x2 [ [a11, a12]
              , [a21, a22]]
  = Matrix2x2 (Vector2D a11 a12)
              (Vector2D a21 a22)

data Matrix =
    Matrix4x4 !Vector !Vector !Vector !Vector
  | Matrix3x3 !Vector !Vector !Vector
  | Matrix2x2 !Vector !Vector
  deriving (Show, Eq, Ord)

instance Semigroup Matrix where
  (<>) x y = x `mul` y

instance Monoid Matrix where
  mempty = identity

makeMatrix :: [[Double]] -> Matrix
makeMatrix xs
  | length xs == 4 = makeMatrix4x4 xs
  | length xs == 3 = makeMatrix3x3 xs
  | length xs == 2 = makeMatrix2x2 xs
  | otherwise = error "Unsupported Matrix size"

identity :: Matrix
identity =
  Matrix4x4
  (Vector4D 1 0 0 0)
  (Vector4D 0 1 0 0)
  (Vector4D 0 0 1 0)
  (Vector4D 0 0 0 1)

newtype RowIndex = RowIndex Int
  deriving (Show, Eq, Ord)

newtype ColumnIndex = ColumnIndex Int
  deriving (Show, Eq, Ord)

getAt :: Matrix -> RowIndex -> ColumnIndex -> Double
getAt (Matrix4x4 x y z w) (RowIndex r) (ColumnIndex c)
  = case r of
      0 -> get x c
      1 -> get y c
      2 -> get z c
      3 -> get w c
getAt (Matrix3x3 x y z) (RowIndex r) (ColumnIndex c)
  = case r of
      0 -> get x c
      1 -> get y c
      2 -> get z c
getAt (Matrix2x2 x y) (RowIndex r) (ColumnIndex c)
  = case r of
      0 -> get x c
      1 -> get y c

mul :: Matrix -> Matrix -> Matrix
mul (Matrix3x3
      (Vector3D a11 a12 a13) (Vector3D a21 a22 a23) (Vector3D a31 a32 a33))
     (Matrix3x3
      (Vector3D b11 b12 b13) (Vector3D b21 b22 b23) (Vector3D b31 b32 b33))
  = Matrix3x3
    (Vector3D ((a11 * b11) + (a12 * b21) + (a13 * b31))
              ((a11 * b12) + (a12 * b22) + (a13 * b32))
              ((a11 * b13) + (a12 * b23) + (a13 * b33)))
    (Vector3D ((a21 * b11) + (a22 * b21) + (a23 * b31))
              ((a21 * b12) + (a22 * b22) + (a23 * b32))
              ((a21 * b13) + (a22 * b23) + (a23 * b33)))
    (Vector3D ((a31 * b11) + (a32 * b21) + (a33 * b31))
              ((a31 * b12) + (a32 * b22) + (a33 * b32))
              ((a31 * b13) + (a32 * b23) + (a33 * b33)))

mul (Matrix4x4
      (Vector4D a11 a12 a13 a14)
      (Vector4D a21 a22 a23 a24)
      (Vector4D a31 a32 a33 a34)
      (Vector4D a41 a42 a43 a44))
     (Matrix4x4
      (Vector4D b11 b12 b13 b14)
      (Vector4D b21 b22 b23 b24)
      (Vector4D b31 b32 b33 b34)
      (Vector4D b41 b42 b43 b44))
  = Matrix4x4
     (Vector4D ((a11 * b11) + (a12 * b21) + (a13 * b31) + (a14 * b41))
               ((a11 * b12) + (a12 * b22) + (a13 * b32) + (a14 * b42))
               ((a11 * b13) + (a12 * b23) + (a13 * b33) + (a14 * b43))
               ((a11 * b14) + (a12 * b24) + (a13 * b34) + (a14 * b44)))
     (Vector4D ((a21 * b11) + (a22 * b21) + (a23 * b31) + (a24 * b41))
               ((a21 * b12) + (a22 * b22) + (a23 * b32) + (a24 * b42))
               ((a21 * b13) + (a22 * b23) + (a23 * b33) + (a24 * b43))
               ((a21 * b14) + (a22 * b24) + (a23 * b34) + (a24 * b44)))
     (Vector4D ((a31 * b11) + (a32 * b21) + (a33 * b31) + (a34 * b41))
               ((a31 * b12) + (a32 * b22) + (a33 * b32) + (a34 * b42))
               ((a31 * b13) + (a32 * b23) + (a33 * b33) + (a34 * b43))
               ((a31 * b14) + (a32 * b24) + (a33 * b34) + (a34 * b44)))
     (Vector4D ((a41 * b11) + (a42 * b21) + (a43 * b31) + (a44 * b41))
               ((a41 * b12) + (a42 * b22) + (a43 * b32) + (a44 * b42))
               ((a41 * b13) + (a42 * b23) + (a43 * b33) + (a44 * b43))
               ((a41 * b14) + (a42 * b24) + (a43 * b34) + (a44 * b44)))

mulT :: Matrix -> T.Tuple -> T.Tuple
mulT (Matrix4x4 a b c d) (T.Tuple x y z w)
  = let tv = Vector4D x y z w
    in T.Tuple (a `dot` tv) (b `dot` tv) (c `dot` tv) (d `dot` tv)

transpose :: Matrix -> Matrix
transpose
  (Matrix4x4
    (Vector4D a11 b12 c13 d14)
    (Vector4D a21 b22 c23 d24)
    (Vector4D a31 b32 c33 d34)
    (Vector4D a41 b42 c43 d44))
  =
  Matrix4x4
  (Vector4D a11 a21 a31 a41)
  (Vector4D b12 b22 b32 b42)
  (Vector4D c13 c23 c33 c43)
  (Vector4D d14 d24 d34 d44)

determinant :: Matrix -> Double
determinant (Matrix2x2 (Vector2D a b) (Vector2D c d)) =  a * d - c * b
determinant m@(Matrix3x3 (Vector3D a b c) _ _)
  =  (a * cofactor m (RowIndex 0) (ColumnIndex 0))
   + (b * cofactor m (RowIndex 0) (ColumnIndex 1))
   + (c * cofactor m (RowIndex 0) (ColumnIndex 2))
determinant m@(Matrix4x4 (Vector4D a b c d) _ _ _)
  =  (a * cofactor m (RowIndex 0) (ColumnIndex 0))
   + (b * cofactor m (RowIndex 0) (ColumnIndex 1))
   + (c * cofactor m (RowIndex 0) (ColumnIndex 2))
   + (d * cofactor m (RowIndex 0) (ColumnIndex 3))

submatrix :: Matrix -> RowIndex -> ColumnIndex -> Matrix
submatrix
  (Matrix4x4 a b c d)
  (RowIndex i) (ColumnIndex j)
  = case i of
      0 -> Matrix3x3
           (dropAt b j)
           (dropAt c j)
           (dropAt d j)
      1 -> Matrix3x3
           (dropAt a j)
           (dropAt c j)
           (dropAt d j)
      2 -> Matrix3x3
           (dropAt a j)
           (dropAt b j)
           (dropAt d j)
      3 -> Matrix3x3
           (dropAt a j)
           (dropAt b j)
           (dropAt c j)
submatrix
  (Matrix3x3 a b c)
  (RowIndex i) (ColumnIndex j)
  = case i of
      0 -> Matrix2x2
           (dropAt b j)
           (dropAt c j)
      1 -> Matrix2x2
           (dropAt a j)
           (dropAt c j)
      2 -> Matrix2x2
           (dropAt a j)
           (dropAt b j)

minor :: Matrix -> RowIndex -> ColumnIndex -> Double
minor a r c = determinant (submatrix a r c)

cofactor :: Matrix -> RowIndex -> ColumnIndex -> Double
cofactor a r@(RowIndex ri) c@(ColumnIndex ci)
  | odd (ri + ci) = - minor a r c
  | otherwise     = minor a r c

invertible :: Matrix -> Bool
invertible a
  | determinant a == 0 = False
  | otherwise          = True

inverse :: Matrix -> Matrix
inverse
  a@(Matrix4x4
     (Vector4D a11 b12 c13 d14)
     (Vector4D a21 b22 c23 d24)
     (Vector4D a31 b32 c33 d34)
     (Vector4D a41 b42 c43 d44))
  | invertible a = makeMatrix4x4
                    [[cofactor a (RowIndex i) (ColumnIndex j) / determinant a
                     | i <- [0..3]] | j <- [0..3]]
  | otherwise     = error "Matrix is not invertible"
