module Matrices
  ( --Matrix
   VMatrix
  --, makeMatrix
  , makeVMatrix
  --, getAt
  , getAtV
  , RowIndex (..)
  , ColumnIndex (..)
  --, mul
  --, mulT
  , mulTV
  , Matrices.mulV
  --, identity
  , identityV
  --, transpose
  , transposeV
  --, determinant
  , determinantV
  --, submatrix
  , submatrixV
  --, minor
  , minorV
  --, cofactor
  , cofactorV
  --, invertible
  , invertibleV
  --, inverse
  , inverseV
  ) where

import qualified Tuples as T
import Utils
import Data.Array.Unboxed
import Vector

----
-- Vector based Matrix

makeVMatrix4x4 [ [a11, a12, a13, a14]
               , [a21, a22, a23, a24]
               , [a31, a32, a33, a34]
               , [a41, a42, a43, a44]]
  = VMatrix4x4 (Vector4D a11 a12 a13 a14)
               (Vector4D a21 a22 a23 a24)
               (Vector4D a31 a32 a33 a34)
               (Vector4D a41 a42 a43 a44)

makeVMatrix3x3 [ [a11, a12, a13]
               , [a21, a22, a23]
               , [a31, a32, a33]]
  = VMatrix3x3 (Vector3D a11 a12 a13)
               (Vector3D a21 a22 a23)
               (Vector3D a31 a32 a33)

makeVMatrix2x2 [ [a11, a12]
               , [a21, a22]]
  = VMatrix2x2 (Vector2D a11 a12)
               (Vector2D a21 a22)

data VMatrix =
    VMatrix4x4 !Vector !Vector !Vector !Vector
  | VMatrix3x3 !Vector !Vector !Vector
  | VMatrix2x2 !Vector !Vector
  deriving (Show, Eq, Ord)

instance Semigroup VMatrix where
  (<>) x y = x `mulV` y

instance Monoid VMatrix where
  mempty = identityV

makeVMatrix :: [[Double]] -> VMatrix
makeVMatrix xs
  | length xs == 4 = makeVMatrix4x4 xs
  | length xs == 3 = makeVMatrix3x3 xs
  | length xs == 2 = makeVMatrix2x2 xs
  | otherwise = error "Unsupported Matrix size"

identityV :: VMatrix
identityV =
  VMatrix4x4
  (Vector4D 1 0 0 0)
  (Vector4D 0 1 0 0)
  (Vector4D 0 0 1 0)
  (Vector4D 0 0 0 1)

newtype RowIndex = RowIndex Int
  deriving (Show, Eq, Ord)

newtype ColumnIndex = ColumnIndex Int
  deriving (Show, Eq, Ord)

getAtV :: VMatrix -> RowIndex -> ColumnIndex -> Double
getAtV (VMatrix4x4 x y z w) (RowIndex r) (ColumnIndex c)
  = case r of
      0 -> get x c
      1 -> get y c
      2 -> get z c
      3 -> get w c
getAtV (VMatrix3x3 x y z) (RowIndex r) (ColumnIndex c)
  = case r of
      0 -> get x c
      1 -> get y c
      2 -> get z c
getAtV (VMatrix2x2 x y) (RowIndex r) (ColumnIndex c)
  = case r of
      0 -> get x c
      1 -> get y c

mulV :: VMatrix -> VMatrix -> VMatrix
mulV (VMatrix3x3
      (Vector3D a11 a12 a13) (Vector3D a21 a22 a23) (Vector3D a31 a32 a33))
     (VMatrix3x3
      (Vector3D b11 b12 b13) (Vector3D b21 b22 b23) (Vector3D b31 b32 b33))
  = VMatrix3x3
    (Vector3D ((a11 * b11) + (a12 * b21) + (a13 * b31))
              ((a11 * b12) + (a12 * b22) + (a13 * b32))
              ((a11 * b13) + (a12 * b23) + (a13 * b33)))
    (Vector3D ((a21 * b11) + (a22 * b21) + (a23 * b31))
              ((a21 * b12) + (a22 * b22) + (a23 * b32))
              ((a21 * b13) + (a22 * b23) + (a23 * b33)))
    (Vector3D ((a31 * b11) + (a32 * b21) + (a33 * b31))
              ((a31 * b12) + (a32 * b22) + (a33 * b32))
              ((a31 * b13) + (a32 * b23) + (a33 * b33)))

mulV (VMatrix4x4
      (Vector4D a11 a12 a13 a14)
      (Vector4D a21 a22 a23 a24)
      (Vector4D a31 a32 a33 a34)
      (Vector4D a41 a42 a43 a44))
     (VMatrix4x4
      (Vector4D b11 b12 b13 b14)
      (Vector4D b21 b22 b23 b24)
      (Vector4D b31 b32 b33 b34)
      (Vector4D b41 b42 b43 b44))
  = VMatrix4x4
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

mulTV :: VMatrix -> T.Tuple -> T.Tuple
mulTV (VMatrix4x4 a b c d) (T.Tuple x y z w)
  = let tv = Vector4D x y z w
    in T.Tuple (a `dot` tv) (b `dot` tv) (c `dot` tv) (d `dot` tv)

transposeV :: VMatrix -> VMatrix
transposeV
  (VMatrix4x4
    (Vector4D a11 b12 c13 d14)
    (Vector4D a21 b22 c23 d24)
    (Vector4D a31 b32 c33 d34)
    (Vector4D a41 b42 c43 d44))
  =
  VMatrix4x4
  (Vector4D a11 a21 a31 a41)
  (Vector4D b12 b22 b32 b42)
  (Vector4D c13 c23 c33 c43)
  (Vector4D d14 d24 d34 d44)

determinantV :: VMatrix -> Double
determinantV (VMatrix2x2 (Vector2D a b) (Vector2D c d)) =  a * d - c * b
determinantV m@(VMatrix3x3 (Vector3D a b c) _ _)
  =  (a * cofactorV m (RowIndex 0) (ColumnIndex 0))
   + (b * cofactorV m (RowIndex 0) (ColumnIndex 1))
   + (c * cofactorV m (RowIndex 0) (ColumnIndex 2))
determinantV m@(VMatrix4x4 (Vector4D a b c d) _ _ _)
  =  (a * cofactorV m (RowIndex 0) (ColumnIndex 0))
   + (b * cofactorV m (RowIndex 0) (ColumnIndex 1))
   + (c * cofactorV m (RowIndex 0) (ColumnIndex 2))
   + (d * cofactorV m (RowIndex 0) (ColumnIndex 3))

submatrixV :: VMatrix -> RowIndex -> ColumnIndex -> VMatrix
submatrixV
  (VMatrix4x4 a b c d)
  (RowIndex i) (ColumnIndex j)
  = case i of
      0 -> VMatrix3x3
           (dropAtV b j)
           (dropAtV c j)
           (dropAtV d j)
      1 -> VMatrix3x3
           (dropAtV a j)
           (dropAtV c j)
           (dropAtV d j)
      2 -> VMatrix3x3
           (dropAtV a j)
           (dropAtV b j)
           (dropAtV d j)
      3 -> VMatrix3x3
           (dropAtV a j)
           (dropAtV b j)
           (dropAtV c j)
submatrixV
  (VMatrix3x3 a b c)
  (RowIndex i) (ColumnIndex j)
  = case i of
      0 -> VMatrix2x2
           (dropAtV b j)
           (dropAtV c j)
      1 -> VMatrix2x2
           (dropAtV a j)
           (dropAtV c j)
      2 -> VMatrix2x2
           (dropAtV a j)
           (dropAtV b j)

minorV :: VMatrix -> RowIndex -> ColumnIndex -> Double
minorV a r c = determinantV (submatrixV a r c)

cofactorV :: VMatrix -> RowIndex -> ColumnIndex -> Double
cofactorV a r@(RowIndex ri) c@(ColumnIndex ci)
  | odd (ri + ci) = - minorV a r c
  | otherwise     = minorV a r c

invertibleV :: VMatrix -> Bool
invertibleV a
  | determinantV a == 0 = False
  | otherwise           = True

inverseV :: VMatrix -> VMatrix
inverseV
  a@(VMatrix4x4
     (Vector4D a11 b12 c13 d14)
     (Vector4D a21 b22 c23 d24)
     (Vector4D a31 b32 c33 d34)
     (Vector4D a41 b42 c43 d44))
  | invertibleV a = makeVMatrix4x4
                    [[cofactorV a (RowIndex i) (ColumnIndex j) / determinantV a
                     | i <- [0..3]] | j <- [0..3]]
  | otherwise     = error "Matrix is not invertible"
