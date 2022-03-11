module Matrices
  ( Matrix
  , makeMatrix
  , getAt
  , RowIndex (..)
  , ColumnIndex (..)
  ) where

data Matrix a = Matrix [[a]]
  deriving (Show, Eq)

makeMatrix :: [[a]] -> Matrix a
makeMatrix [ [a11, a12, a13, a14]
           , [a21, a22, a23, a24]
           , [a31, a32, a33, a34]
           , [a41, a42, a43, a44]]
  = Matrix [ [a11, a12, a13, a14]
           , [a21, a22, a23, a24]
           , [a31, a32, a33, a34]
           , [a41, a42, a43, a44]]
makeMatrix _ = error "Unsupported Matrix size"

newtype RowIndex = RowIndex Int
  deriving (Show, Eq, Ord)

newtype ColumnIndex = ColumnIndex Int
  deriving (Show, Eq, Ord)

getAt :: Matrix a -> RowIndex -> ColumnIndex -> a
getAt (Matrix m) (RowIndex r) (ColumnIndex c) = (m !! r) !! c
