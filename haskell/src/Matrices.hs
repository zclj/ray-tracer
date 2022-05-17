module Matrices
  ( Matrix
  , makeMatrix
  , makeMatrix4x4
  , makeMatrix3x3
  , makeUMatrix
  , getAt
  , getAtU
  , RowIndex (..)
  , ColumnIndex (..)
  , mul
  , mulT
  , mulTU
  , mulU
  , identity
  , identityU
  , transpose
  , transposeU
  , determinant
  , determinantU
  , submatrix
  , submatrixU
  , minor
  , minorU
  , cofactor
  , cofactorU
  , invertible
  , inverse
  ) where

import qualified Tuples as T
import Utils
import Data.Array.Unboxed

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
data UMatrix = UMatrix (UArray (Int, Int) Double)
  deriving(Show)

eqUMatrix :: UMatrix -> UMatrix -> Bool
eqUMatrix (UMatrix x) (UMatrix y)
  = let ((lix,ljx), (uix,ujx)) = bounds x
        ((liy,ljy), (uiy,ujy)) = bounds y
        result                 = [[abs (x!(i,k) - y!(i,k)) < 0.0001 | k <- [ljx..ujx]]
                                 | i <- [lix..uix]]
    in (bounds x) == (bounds y) && (and $ concat result)

instance Eq UMatrix where
  x == y = eqUMatrix x y

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

newtype RowIndex = RowIndex Int
  deriving (Show, Eq, Ord)

newtype ColumnIndex = ColumnIndex Int
  deriving (Show, Eq, Ord)

getAt :: Matrix -> RowIndex -> ColumnIndex -> Double
getAt (Matrix m) (RowIndex r) (ColumnIndex c) = (m !! r) !! c

getAtU :: UMatrix -> RowIndex -> ColumnIndex -> Double
getAtU (UMatrix m) (RowIndex r) (ColumnIndex c) = m!(r,c)

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

transpose :: Matrix -> Matrix
transpose a = Matrix [[getAt a (RowIndex j) (ColumnIndex i) | j <- [0..3]] | i <- [0..3]]

transposeU :: UMatrix -> UMatrix
transposeU (UMatrix a) = UMatrix (array ((0,0),(3,3))
                                  [((i,j), a!(j,i)) | j <- [0..3], i <- [0..3]])

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

-- submatrix :: Matrix -> RowIndex -> ColumnIndex -> Matrix
-- submatrix (Matrix a) (RowIndex r) (ColumnIndex c)
--   = let subRows = dropAt r a
--         subCols = map (dropAt c) subRows
--     in Matrix subCols

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
                    , ((1,0),m!(2,0)), ((1,1),m!(2,1)), ((1,2),m!(2,1)), ((1,3),m!(2,3))
                    , ((2,0),m!(3,0)), ((2,1),m!(3,1)), ((2,2),m!(3,2)), ((2,3),m!(3,3))]
               1 -> [ ((0,0),m!(0,0)), ((0,1),m!(0,1)), ((0,2),m!(0,2)), ((0,3),m!(0,3))
                    , ((1,0),m!(2,0)), ((1,1),m!(2,1)), ((1,2),m!(2,1)), ((1,3),m!(2,3))
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

test = submatrix (makeMatrix [[1,1,1],[2,2,2], [3,3,3]]) (RowIndex 0) (ColumnIndex 0)
test2 = dropAt 0 [[1,1,1],[2,2,2], [3,3,3]]

minor :: Matrix -> RowIndex -> ColumnIndex -> Double
minor a r = determinant . submatrix a r

minorU :: UMatrix -> RowIndex -> ColumnIndex -> Double
minorU a r = determinantU . submatrixU a r

cofactor :: Matrix -> RowIndex -> ColumnIndex -> Double
cofactor a r@(RowIndex ri) c@(ColumnIndex ci)
  | odd (ri + ci) = - minor a r c
  | otherwise     = minor a r c

cofactorU :: UMatrix -> RowIndex -> ColumnIndex -> Double
cofactorU a r@(RowIndex ri) c@(ColumnIndex ci)
  | odd (ri + ci) = - minorU a r c
  | otherwise     = minorU a r c

invertible :: Matrix -> Bool
invertible a
  | determinant a == 0 = False
  | otherwise          = True

inverse :: Matrix -> Matrix
inverse a
  | invertible a = let det       = determinant a
                                   -- note that the transpose is implicit, indexes
                                   -- are changed, i.e., (i,j) -> (j,i)
                       cofactors = [[cofactor a (RowIndex i) (ColumnIndex j) / det
                                    | i <- [0..3]] | j <- [0..3]]
                   in Matrix cofactors
  | otherwise    = error "Matrix is not invertible"

