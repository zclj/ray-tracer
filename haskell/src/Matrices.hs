module Matrices
  ( Matrix
  , makeMatrix
  , makeMatrix4x4
  , makeMatrix3x3
  , getAt
  , RowIndex (..)
  , ColumnIndex (..)
  , mul
  , mulT
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
import Utils

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

newtype RowIndex = RowIndex Int
  deriving (Show, Eq, Ord)

newtype ColumnIndex = ColumnIndex Int
  deriving (Show, Eq, Ord)

getAt :: Matrix -> RowIndex -> ColumnIndex -> Double
getAt (Matrix m) (RowIndex r) (ColumnIndex c) = (m !! r) !! c

mul :: Matrix -> Matrix -> Matrix
mul a b =
  let get = (\m r c -> getAt m (RowIndex r) (ColumnIndex c))
      rxc = (\m1 m2 r c k -> get m1 r k * get m2 k c)
  in Matrix [[sum (map (rxc a b i j) [0..3]) | j <- [0..3]] | i <- [0..3]]

mulT :: Matrix -> T.Tuple -> T.Tuple
mulT a@(Matrix m) b = let get = (\m r c -> getAt m (RowIndex r) (ColumnIndex c))
                          tupleFromList [x, y, z, w] = T.Tuple x y z w
                      in tupleFromList [tupleFromList (m !! i) `T.dot` b
                                       | i <- [0..3]]

transpose :: Matrix -> Matrix
transpose a = Matrix [[getAt a (RowIndex j) (ColumnIndex i) | j <- [0..3]] | i <- [0..3]]

determinant :: Matrix -> Double
determinant (Matrix [[a, b], [c, d]]) = a * d - c * b
determinant m@(Matrix x) =
  let onRowZero = (\f c -> f m (RowIndex 0) (ColumnIndex c))
      size      = length (head x) - 1
  in sum $ map (\j -> onRowZero getAt j * onRowZero cofactor j) [0..size]

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

test = submatrix (makeMatrix [[1,1,1],[2,2,2], [3,3,3]]) (RowIndex 0) (ColumnIndex 0)
test2 = dropAt 0 [[1,1,1],[2,2,2], [3,3,3]]

minor :: Matrix -> RowIndex -> ColumnIndex -> Double
minor a r = determinant . submatrix a r

cofactor :: Matrix -> RowIndex -> ColumnIndex -> Double
cofactor a r@(RowIndex ri) c@(ColumnIndex ci)
  | odd (ri + ci) = - minor a r c
  | otherwise     = minor a r c

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

