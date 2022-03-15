module Matrices
  ( Matrix
  , makeMatrix
  , getAt
  , RowIndex (..)
  , ColumnIndex (..)
  , mul
  ) where

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

newtype RowIndex = RowIndex Int
  deriving (Show, Eq, Ord)

newtype ColumnIndex = ColumnIndex Int
  deriving (Show, Eq, Ord)

getAt :: Matrix a -> RowIndex -> ColumnIndex -> a
getAt (Matrix m) (RowIndex r) (ColumnIndex c) = (m !! r) !! c

mul :: Matrix a -> Matrix a -> Matrix a
mul (Matrix a) (Matrix b) = undefined
