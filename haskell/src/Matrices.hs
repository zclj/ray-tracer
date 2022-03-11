module Matrices where

data Matrix = Matrix [[Double]]
  deriving (Show, Eq)

makeMatrix [ [a11, a12, a13, a14]
           , [a21, a22, a23, a24]
           , [a31, a32, a33, a34]
           , [a41, a42, a43, a44]]
  = Matrix [ [a11, a12, a13, a14]
           , [a21, a22, a23, a24]
           , [a31, a32, a33, a34]
           , [a41, a42, a43, a44]]

makeMatrix _ = error "Unsupported Matrix size"
