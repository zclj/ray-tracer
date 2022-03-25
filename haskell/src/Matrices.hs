module Matrices
  ( Matrix
  , makeMatrix
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
  ) where

import qualified Tuples as T

data Matrix a = Matrix [[a]]
  deriving (Show)

-- Implement Eq based on Epsilon comparison of floats
-- this avoids two floats that are "equal" being evaluated as not equal
-- due too how floats are represented
instance (Eq a, Num a, Ord a, Fractional a) => Eq (Matrix a) where
  (Matrix x) == (Matrix y)
    = let ltep    = (\(x,y) -> abs (x - y) < epsilon)
          epsilon = 0.0001
      in all ltep $ zip (concat x) (concat y)

makeMatrix :: [[a]] -> Matrix a
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

identity :: Matrix Double
identity = Matrix [[1, 0, 0, 0], [0, 1, 0, 0],
                   [0, 0, 1, 0], [0, 0, 0, 1]]

newtype RowIndex = RowIndex Int
  deriving (Show, Eq, Ord)

newtype ColumnIndex = ColumnIndex Int
  deriving (Show, Eq, Ord)

getAt :: Matrix a -> RowIndex -> ColumnIndex -> a
getAt (Matrix m) (RowIndex r) (ColumnIndex c) = (m !! r) !! c

mul :: Num a => Matrix a -> Matrix a -> Matrix a
mul a b =
  let get = (\m r c -> getAt m (RowIndex r) (ColumnIndex c))
      rxc = (\m1 m2 r c k -> get m1 r k * get m2 k c)
  in Matrix [[sum (map (rxc a b i j) [0..3]) | j <- [0..3]] | i <- [0..3]]

mulT :: Matrix Double -> T.Tuple -> T.Tuple
mulT a@(Matrix m) b = let get = (\m r c -> getAt m (RowIndex r) (ColumnIndex c))
                          tupleFromList [x, y, z, w] = T.Tuple x y z w
                      in tupleFromList [tupleFromList (m !! i) `T.dot` b
                                       | i <- [0..3]]

transpose :: Matrix Double -> Matrix Double
transpose a = Matrix [[getAt a (RowIndex j) (ColumnIndex i) | j <- [0..3]] | i <- [0..3]]

determinant :: Matrix Double -> Double
determinant (Matrix [[a, b], [c, d]]) = a * d - c * b
determinant m@(Matrix x) =
  let onRowZero = (\f c -> f m (RowIndex 0) (ColumnIndex c))
      size      = length (head x) - 1
  in sum $ map (\j -> onRowZero getAt j * onRowZero cofactor j) [0..size]

dropAt :: Int -> [a] -> [a]
dropAt i xs = pre ++ tail post
  where (pre, post) = splitAt i xs

submatrix :: Matrix Double -> RowIndex -> ColumnIndex -> Matrix Double
submatrix (Matrix a) (RowIndex r) (ColumnIndex c)
  = let subRows = dropAt r a
        subCols = map (dropAt c) subRows
    in Matrix subCols

minor :: Matrix Double -> RowIndex -> ColumnIndex -> Double
minor a r = determinant . submatrix a r

cofactor :: Matrix Double -> RowIndex -> ColumnIndex -> Double
cofactor a r@(RowIndex ri) c@(ColumnIndex ci)
  | odd (ri + ci) = - minor a r c
  | otherwise     = minor a r c

invertible :: Matrix Double -> Bool
invertible a
  | (determinant a) == 0 = False
  | otherwise            = True
