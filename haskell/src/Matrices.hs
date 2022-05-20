module Matrices
  ( Matrix
  , UMatrix
  , makeMatrix
  , makeMatrix4x4
  , makeMatrix3x3
  , makeUMatrix
  , makeVMatrix
  , getAt
  , getAtU
  , getAtV
  , RowIndex (..)
  , ColumnIndex (..)
  , mul
  , mulT
  , mulTU
  , mulTV
  , mulU
  , Matrices.mulV
  , identity
  , identityU
  , identityV
  , transpose
  , transposeU
  , transposeV
  , determinant
  , determinantU
  , determinantV
  , submatrix
  , submatrixU
  , submatrixV
  , minor
  , minorU
  , minorV
  , cofactor
  , cofactorU
  , cofactorV
  , invertible
  , invertibleU
  , inverse
  , inverseU
  ) where

import qualified Tuples as T
import Utils
import Data.Array.Unboxed
import Vector

data Matrix = Matrix [[Double]]
  deriving (Show, Ord)

-- Implement Eq based on Epsilon comparison of floats
-- this avoids two floats that are "equal" being evaluated as not equal
-- due too how floats are represented
instance Eq Matrix where
  (Matrix x) == (Matrix y)
    = let ltep    = (\(x,y) -> abs (x - y) < epsilon)
          epsilon = 0.0001
      in all ltep $ zip (concat x) (concat y)

instance Semigroup Matrix where
  (<>) x y = x `mul` y

instance Monoid Matrix where
  mempty = identity

makeMatrix :: [[Double]] -> Matrix
makeMatrix [ [a11, a12, a13, a14]
           , [a21, a22, a23, a24]
           , [a31, a32, a33, a34]
           , [a41, a42, a43, a44]]
  = Matrix [ [a11, a12, a13, a14]
           , [a21, a22, a23, a24]
           , [a31, a32, a33, a34]
           , [a41, a42, a43, a44]]
makeMatrix [ [a11, a12, a13]
           , [a21, a22, a23]
           , [a31, a32, a33]]
  = Matrix [ [a11, a12, a13]
           , [a21, a22, a23]
           , [a31, a32, a33]]
makeMatrix [ [a11, a12]
           , [a21, a22]]
  = Matrix [ [a11, a12]
           , [a21, a22]]
makeMatrix _ = error "Unsupported Matrix size"

----
-- matrix with arrays

-- https://www.haskell.org/tutorial/arrays.html
-- https://hackage.haskell.org/package/base-4.16.1.0/docs/GHC-Arr.html#v:array
-- https://stackoverflow.com/questions/5522074/taking-sub-arrays-in-haskell
-- https://stackoverflow.com/questions/11768656/reasonably-efficient-pure-functional-matrix-product-in-haskell

-- https://github.com/ekmett/linear/blob/2384ebbe12fe68d397f23d8a6ffe8278da7073da/src/Linear/V4.hs#L35

-- https://hackage.haskell.org/package/vector-0.5/docs/Data-Vector-Storable.html

-- https://github.com/haskell-numerics/hmatrix
data UMatrix = UMatrix (UArray (Int, Int) Double)
  deriving(Show, Ord)

eqUMatrix :: UMatrix -> UMatrix -> Bool
eqUMatrix (UMatrix x) (UMatrix y)
  = let ((lix,ljx), (uix,ujx)) = bounds x
        ((liy,ljy), (uiy,ujy)) = bounds y
        result                 = [[abs (x!(i,k) - y!(i,k)) < 0.0001 | k <- [ljx..ujx]]
                                 | i <- [lix..uix]]
    in (bounds x) == (bounds y) && (and $ concat result)

instance Eq UMatrix where
  x == y = eqUMatrix x y

instance Semigroup UMatrix where
  (<>) x y = x `mulU` y

instance Monoid UMatrix where
  mempty = identityU

m1 = makeUMatrix [[1, 2], [3,4]]
m2 = makeUMatrix [[1, 2], [3,4]]
m3 = makeUMatrix [[2, 2], [4,4]]
m4 = makeUMatrix [[2, 2.001], [4,4]]
m5 = makeUMatrix [[2, 2.00001], [4,4]]

makeUMatrix :: [[Double]] -> UMatrix
makeUMatrix [ [a11, a12, a13, a14]
            , [a21, a22, a23, a24]
            , [a31, a32, a33, a34]
            , [a41, a42, a43, a44]]
  = UMatrix (array ((0,0), (3,3)) [ ((0,0),a11), ((0,1),a12), ((0,2),a13), ((0,3),a14)
                                  , ((1,0),a21), ((1,1),a22), ((1,2),a23), ((1,3),a24)
                                  , ((2,0),a31), ((2,1),a32), ((2,2),a33), ((2,3),a34)
                                  , ((3,0),a41), ((3,1),a42), ((3,2),a43), ((3,3),a44)])
makeUMatrix [ [a11, a12, a13]
            , [a21, a22, a23]
            , [a31, a32, a33]]
  = UMatrix (array ((0,0), (2,2)) [ ((0,0),a11), ((0,1),a12), ((0,2),a13)
                                  , ((1,0),a21), ((1,1),a22), ((1,2),a23)
                                  , ((2,0),a31), ((2,1),a32), ((2,2),a33)])
makeUMatrix [ [a11, a12]
            , [a21, a22]]
  = UMatrix (array ((0,0), (1,1)) [ ((0,0),a11), ((0,1),a12)
                                  , ((1,0),a21), ((1,1),a22)])
makeUMatrix _ = error "Unsupported UMatrix size"

----
-- Specialized matrix

data Matrix4x4 = Matrix4x4 [[Double]]

makeMatrix4x4 [ [a11, a12, a13, a14]
              , [a21, a22, a23, a24]
              , [a31, a32, a33, a34]
              , [a41, a42, a43, a44]]
  = Matrix4x4 [ [a11, a12, a13, a14]
              , [a21, a22, a23, a24]
              , [a31, a32, a33, a34]
              , [a41, a42, a43, a44]]

data Matrix3x3 = Matrix3x3 [[Double]]

makeMatrix3x3 [ [a11, a12, a13]
              , [a21, a22, a23]
              , [a31, a32, a33]]
  = Matrix3x3 [ [a11, a12, a13]
              , [a21, a22, a23]
              , [a31, a32, a33]]

----

----
-- Vector based Matrix

-- data VMatrix4x4 = VMatrix4x4 !Vector4D !Vector4D !Vector4D !Vector4D

makeVMatrix4x4 [ [a11, a12, a13, a14]
               , [a21, a22, a23, a24]
               , [a31, a32, a33, a34]
               , [a41, a42, a43, a44]]
  = VMatrix4x4 (Vector4D a11 a12 a13 a14)
               (Vector4D a21 a22 a23 a24)
               (Vector4D a31 a32 a33 a34)
               (Vector4D a41 a42 a43 a44)

-- data VMatrix3x3 = VMatrix3x3 !Vector3D !Vector3D !Vector3D

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
  deriving (Show, Eq)

makeVMatrix :: [[Double]] -> VMatrix
makeVMatrix xs
  | length xs == 4 = makeVMatrix4x4 xs
  | length xs == 3 = makeVMatrix3x3 xs
  | length xs == 2 = makeVMatrix2x2 xs
  | otherwise = error "Unsupported Matrix size"
----

identity :: Matrix
identity = Matrix [[1, 0, 0, 0], [0, 1, 0, 0],
                   [0, 0, 1, 0], [0, 0, 0, 1]]

identityU :: UMatrix
identityU = UMatrix
  (array ((0,0), (3,3))
    [ ((0,0),1), ((0,1),0), ((0,2),0), ((0,3),0)
    , ((1,0),0), ((1,1),1), ((1,2),0), ((1,3),0)
    , ((2,0),0), ((2,1),0), ((2,2),1), ((2,3),0)
    , ((3,0),0), ((3,1),0), ((3,2),0), ((3,3),1)])

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

getAt :: Matrix -> RowIndex -> ColumnIndex -> Double
getAt (Matrix m) (RowIndex r) (ColumnIndex c) = (m !! r) !! c

getAtU :: UMatrix -> RowIndex -> ColumnIndex -> Double
getAtU (UMatrix m) (RowIndex r) (ColumnIndex c) = m!(r,c)

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

mul :: Matrix -> Matrix -> Matrix
mul a b =
  let get = (\m r c -> getAt m (RowIndex r) (ColumnIndex c))
      rxc = (\m1 m2 r c k -> get m1 r k * get m2 k c)
  in Matrix [[sum (map (rxc a b i j) [0..3]) | j <- [0..3]] | i <- [0..3]]

mulU :: UMatrix -> UMatrix -> UMatrix
mulU (UMatrix a) (UMatrix b) =
  let resultBounds  = bounds a
      (ri, rj) = resultBounds
      result = array resultBounds
                     [((i,j), sum [a!(i,k) * b!(k,j) | k <- [0..3]])
                                  | i <- [0..3],
                                    j <- [0..3]]
  in UMatrix result

mulV :: VMatrix -> VMatrix -> VMatrix
mulV (VMatrix3x3
      (Vector3D a11 a12 a13) (Vector3D a21 a22 a23) (Vector3D a31 a32 a33))
     (VMatrix3x3
      (Vector3D b11 b12 b13) (Vector3D b21 b22 b23) (Vector3D b31 b32 b33))
  = (VMatrix3x3
     (Vector3D ((a11 * b11) + (a12 * b21) + (a13 * b31))
               ((a11 * b12) + (a12 * b22) + (a13 * b32))
               ((a11 * b13) + (a12 * b23) + (a13 * b33)))
     (Vector3D ((a21 * b11) + (a22 * b21) + (a23 * b31))
               ((a21 * b12) + (a22 * b22) + (a23 * b32))
               ((a21 * b13) + (a22 * b23) + (a23 * b33)))
     (Vector3D ((a31 * b11) + (a32 * b21) + (a33 * b31))
               ((a31 * b12) + (a32 * b22) + (a33 * b32))
               ((a31 * b13) + (a32 * b23) + (a33 * b33))))

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
  = (VMatrix4x4
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
               ((a41 * b14) + (a42 * b24) + (a43 * b34) + (a44 * b44))))

mulT :: Matrix -> T.Tuple -> T.Tuple
mulT a@(Matrix m) b = let get = (\m r c -> getAt m (RowIndex r) (ColumnIndex c))
                          tupleFromList [x, y, z, w] = T.Tuple x y z w
                      in tupleFromList [tupleFromList (m !! i) `T.dot` b
                                       | i <- [0..3]]

matrixRow :: UMatrix -> Int -> [Double]
matrixRow (UMatrix m) r = let ((_, _), (_, uj)) = bounds m
                          in [m!(r,i) | i <- [0..uj]]

mulTU :: UMatrix -> T.Tuple -> T.Tuple
mulTU a@(UMatrix m) b =
  let (ri, rj)                   = bounds m
      tupleFromList [x, y, z, w] = T.Tuple x y z w
      result                     = [ tupleFromList (matrixRow a i) `T.dot` b | i <- [0..3]]
  in tupleFromList result

mulTV :: VMatrix -> T.Tuple -> T.Tuple
mulTV (VMatrix4x4 a b c d) (T.Tuple x y z w)
  = let tv = Vector4D x y z w
    in (T.Tuple (a `dot` tv) (b `dot` tv) (c `dot` tv) (d `dot` tv))

transpose :: Matrix -> Matrix
transpose a = Matrix [[getAt a (RowIndex j) (ColumnIndex i) | j <- [0..3]] | i <- [0..3]]

transposeU :: UMatrix -> UMatrix
transposeU (UMatrix a) = UMatrix (array ((0,0),(3,3))
                                  [((i,j), a!(j,i)) | j <- [0..3], i <- [0..3]])

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

determinant :: Matrix -> Double
determinant (Matrix [[a, b], [c, d]]) = a * d - c * b
determinant m@(Matrix x) =
  let onRowZero = (\f c -> f m (RowIndex 0) (ColumnIndex c))
      size      = length (head x) - 1
  in sum $ map (\j -> onRowZero getAt j * onRowZero cofactor j) [0..size]

determinantU :: UMatrix -> Double
determinantU a@(UMatrix m) =
  let ((li,hi), (lj, hj)) = bounds m
  in case hj of
    1 -> (m!(0,0) * m!(1,1)) - (m!(1,0) * m!(0,1))
    otherwise -> sum $ map (\j -> m!(0,j) * (cofactorU a (RowIndex 0) (ColumnIndex j))) [0..hj]

determinantV :: VMatrix -> Double
determinantV (VMatrix2x2 (Vector2D a b) (Vector2D c d)) =  a * d - c * b

subColsX :: Int -> Matrix -> Matrix
subColsX i
  (Matrix [[x1,y1,z1,w1], [x2,y2,z2,w2], [x3,y3,z3,w3]])
  = let xs = case i of
               0 -> [[y1,z1,w1]
                    ,[y2,z2,w2]
                    ,[y3,z3,w3]]
               1 -> [[x1,z1,w1]
                    ,[x2,z2,w2]
                    ,[x3,z3,w3]]
               2 -> [[x1,y1,w1]
                    ,[x2,y2,w2]
                    ,[x3,y3,w3]]
               3 -> [[x1,y1,z1]
                    ,[x2,y2,z2]
                    ,[x3,y3,z3]]
    in makeMatrix xs
subColsX i
  (Matrix [[x1,y1,z1], [x2,y2,z2]])
  = let xs = case i of
               0 -> [[y1,z1]
                    ,[y2,z2]]
               1 -> [[x1,z1]
                    ,[x2,z2]]
               2 -> [[x1,y1]
                    ,[x2,y2]]
    in makeMatrix xs
  
-- 'deletes' the given row and column from the given matrix
submatrix :: Matrix -> RowIndex -> ColumnIndex -> Matrix
submatrix (Matrix a) (RowIndex r) (ColumnIndex c)
  = let subRows = dropAt r a
        subCols = subColsX c (Matrix subRows)
    in subCols

dropRow4x4 :: UMatrix -> Int -> UMatrix
dropRow4x4 (UMatrix m) i
  = let xs = case i of
               0 -> [ ((0,0),m!(1,0)), ((0,1),m!(1,1)), ((0,2),m!(1,2)), ((0,3),m!(1,3))
                    , ((1,0),m!(2,0)), ((1,1),m!(2,1)), ((1,2),m!(2,2)), ((1,3),m!(2,3))
                    , ((2,0),m!(3,0)), ((2,1),m!(3,1)), ((2,2),m!(3,2)), ((2,3),m!(3,3))]
               1 -> [ ((0,0),m!(0,0)), ((0,1),m!(0,1)), ((0,2),m!(0,2)), ((0,3),m!(0,3))
                    , ((1,0),m!(2,0)), ((1,1),m!(2,1)), ((1,2),m!(2,2)), ((1,3),m!(2,3))
                    , ((2,0),m!(3,0)), ((2,1),m!(3,1)), ((2,2),m!(3,2)), ((2,3),m!(3,3))]
               2 -> [ ((0,0),m!(0,0)), ((0,1),m!(0,1)), ((0,2),m!(0,2)), ((0,3),m!(0,3))
                    , ((1,0),m!(1,0)), ((1,1),m!(1,1)), ((1,2),m!(1,2)), ((1,3),m!(1,3))
                    , ((2,0),m!(3,0)), ((2,1),m!(3,1)), ((2,2),m!(3,2)), ((2,3),m!(3,3))]
               3 -> [ ((0,0),m!(0,0)), ((0,1),m!(0,1)), ((0,2),m!(0,2)), ((0,3),m!(0,3))
                    , ((1,0),m!(1,0)), ((1,1),m!(1,1)), ((1,2),m!(1,2)), ((1,3),m!(1,3))
                    , ((2,0),m!(2,0)), ((2,1),m!(2,1)), ((2,2),m!(2,2)), ((2,3),m!(2,3))]
    in UMatrix (array ((0,0), (2,3)) xs)

dropRow3x3 :: UMatrix -> Int -> UMatrix
dropRow3x3 (UMatrix m) i
  = let xs = case i of
               0 -> [ ((0,0),m!(1,0)), ((0,1),m!(1,1)), ((0,2),m!(1,2))
                    , ((1,0),m!(2,0)), ((1,1),m!(2,1)), ((1,2),m!(2,2))]
               1 -> [ ((0,0),m!(0,0)), ((0,1),m!(0,1)), ((0,2),m!(0,2))
                    , ((1,0),m!(2,0)), ((1,1),m!(2,1)), ((1,2),m!(2,2))]
               2 -> [ ((0,0),m!(0,0)), ((0,1),m!(0,1)), ((0,2),m!(0,2))
                    , ((1,0),m!(1,0)), ((1,1),m!(1,1)), ((1,2),m!(1,2))]
    in UMatrix (array ((0,0), (1,2)) xs)

dropCol3x4 :: UMatrix -> Int -> UMatrix
dropCol3x4 (UMatrix m) j
  = let xs = case j of
               0 -> [ ((0,0),m!(0,1)), ((0,1),m!(0,2)), ((0,2),m!(0,3))
                    , ((1,0),m!(1,1)), ((1,1),m!(1,2)), ((1,2),m!(1,3))
                    , ((2,0),m!(2,1)), ((2,1),m!(2,2)), ((2,2),m!(2,3))]
               1 -> [ ((0,0),m!(0,0)), ((0,1),m!(0,2)), ((0,2),m!(0,3))
                    , ((1,0),m!(1,0)), ((1,1),m!(1,2)), ((1,2),m!(1,3))
                    , ((2,0),m!(2,0)), ((2,1),m!(2,2)), ((2,2),m!(2,3))]
               2 -> [ ((0,0),m!(0,0)), ((0,1),m!(0,1)), ((0,2),m!(0,3))
                    , ((1,0),m!(1,0)), ((1,1),m!(1,1)), ((1,2),m!(1,3))
                    , ((2,0),m!(2,0)), ((2,1),m!(2,1)), ((2,2),m!(2,3))]
               3 -> [ ((0,0),m!(0,0)), ((0,1),m!(0,1)), ((0,2),m!(0,2))
                    , ((1,0),m!(1,0)), ((1,1),m!(1,1)), ((1,2),m!(1,2))
                    , ((2,0),m!(2,0)), ((2,1),m!(2,1)), ((2,2),m!(2,2))]
    in UMatrix (array ((0,0), (2,2)) xs)

dropCol2x3 :: UMatrix -> Int -> UMatrix
dropCol2x3 (UMatrix m) j
  = let xs = case j of
               0 -> [ ((0,0),m!(0,1)), ((0,1),m!(0,2))
                    , ((1,0),m!(1,1)), ((1,1),m!(1,2))]
               1 -> [ ((0,0),m!(0,0)), ((0,1),m!(0,2))
                    , ((1,0),m!(1,0)), ((1,1),m!(1,2))]
               2 -> [ ((0,0),m!(0,0)), ((0,1),m!(0,1))
                    , ((1,0),m!(1,0)), ((1,1),m!(1,1))]
    in UMatrix (array ((0,0), (1,1)) xs)

au = makeUMatrix [[- 6, 1, 1, 6], [- 8, 5, 8, 6],
                  [- 1, 0, 8, 2], [- 7, 1, -1, 1]]

su = submatrixU au (RowIndex 2) (ColumnIndex 1)

bu = makeUMatrix [[- 6, 1, 6], [- 8, 8, 6], [- 7, - 1, 1]]

-- https://github.com/haskell-numerics/hmatrix/blob/master/packages/base/src/Internal/Matrix.hs#L383
submatrix4x4 :: UMatrix -> Int -> Int -> UMatrix
submatrix4x4 a@(UMatrix m) i j = 
  let woRow = dropRow4x4 a i
      woCol = dropCol3x4 woRow j
  in woCol
  
submatrix3x3 :: UMatrix -> Int -> Int -> UMatrix
submatrix3x3 a@(UMatrix m) i j =
  let woRow = dropRow3x3 a i
      woCol = dropCol2x3 woRow j
  in woCol

submatrixU :: UMatrix -> RowIndex -> ColumnIndex -> UMatrix
submatrixU a@(UMatrix m) (RowIndex i) (ColumnIndex j) =
  let ((li,ui), (lj,uj)) = bounds m
      woCol = case uj of
                2 -> submatrix3x3 a i j
                3 -> submatrix4x4 a i j
  in woCol

submatrixV :: VMatrix -> RowIndex -> ColumnIndex -> VMatrix
submatrixV
  (VMatrix4x4 a b c d)
  (RowIndex i) (ColumnIndex j)
  = case i of
      0 -> (VMatrix3x3
            (dropAtV b j)
            (dropAtV c j)
            (dropAtV d j))
      1 -> (VMatrix3x3
            (dropAtV a j)
            (dropAtV c j)
            (dropAtV d j))
      2 -> (VMatrix3x3
            (dropAtV a j)
            (dropAtV b j)
            (dropAtV d j))
      3 -> (VMatrix3x3
            (dropAtV a j)
            (dropAtV b j)
            (dropAtV c j))
submatrixV
  (VMatrix3x3 a b c)
  (RowIndex i) (ColumnIndex j)
  = case i of
      0 -> (VMatrix2x2
            (dropAtV b j)
            (dropAtV c j))
      1 -> (VMatrix2x2
            (dropAtV a j)
            (dropAtV c j))
      2 -> (VMatrix2x2
            (dropAtV a j)
            (dropAtV b j))

test = submatrix (makeMatrix [[1,1,1],[2,2,2], [3,3,3]]) (RowIndex 0) (ColumnIndex 0)
test2 = dropAt 0 [[1,1,1],[2,2,2], [3,3,3]]

minor :: Matrix -> RowIndex -> ColumnIndex -> Double
minor a r = determinant . submatrix a r

minorU :: UMatrix -> RowIndex -> ColumnIndex -> Double
minorU a r = determinantU . submatrixU a r

minorV :: VMatrix -> RowIndex -> ColumnIndex -> Double
minorV a r = determinantV . submatrixV a r

cofactor :: Matrix -> RowIndex -> ColumnIndex -> Double
cofactor a r@(RowIndex ri) c@(ColumnIndex ci)
  | odd (ri + ci) = - minor a r c
  | otherwise     = minor a r c

cofactorU :: UMatrix -> RowIndex -> ColumnIndex -> Double
cofactorU a r@(RowIndex ri) c@(ColumnIndex ci)
  | odd (ri + ci) = - minorU a r c
  | otherwise     = minorU a r c

cofactorV :: VMatrix -> RowIndex -> ColumnIndex -> Double
cofactorV a r@(RowIndex ri) c@(ColumnIndex ci)
  | odd (ri + ci) = - minorV a r c
  | otherwise     = minorV a r c

invertible :: Matrix -> Bool
invertible a
  | determinant a == 0 = False
  | otherwise          = True

invertibleU :: UMatrix -> Bool
invertibleU a
  | determinantU a == 0 = False
  | otherwise           = True

inverse :: Matrix -> Matrix
inverse a
  | invertible a = let det       = determinant a
                                   -- note that the transpose is implicit, indexes
                                   -- are changed, i.e., (i,j) -> (j,i)
                       cofactors = [[cofactor a (RowIndex i) (ColumnIndex j) / det
                                    | i <- [0..3]] | j <- [0..3]]
                   in Matrix cofactors
  | otherwise    = error "Matrix is not invertible"

inverseU :: UMatrix -> UMatrix
inverseU a
  | invertibleU a = let det       = determinantU a
                                   -- note that the transpose is implicit, indexes
                                   -- are changed, i.e., (i,j) -> (j,i)
                        cofactors = [[cofactorU a (RowIndex i) (ColumnIndex j) / det
                                     | i <- [0..3]] | j <- [0..3]]
                   in makeUMatrix cofactors
  | otherwise    = error "Matrix is not invertible"

