module Transformations
  ( translation
  , scaling
  , rotationX
  , rotationY
  , rotationZ
  , shearing
  , transform
  ) where

import Data.Foldable
import Matrices

translation :: Double -> Double -> Double -> UMatrix
translation x y z = makeUMatrix [[1, 0, 0, x],
                                 [0, 1, 0, y],
                                 [0, 0, 1, z],
                                 [0, 0, 0, 1]]

scaling :: Double -> Double -> Double -> UMatrix
scaling x y z = makeUMatrix [[x, 0, 0, 0],
                             [0, y, 0, 0],
                             [0, 0, z, 0],
                             [0, 0, 0, 1]]

rotationX :: Double -> UMatrix
rotationX r = makeUMatrix [[1, 0,     0,       0],
                           [0, cos r, - sin r, 0],
                           [0, sin r, cos r,   0],
                           [0, 0,     0,       1]]

rotationY :: Double -> UMatrix
rotationY r = makeUMatrix [[cos r,   0, sin r, 0],
                           [0,       1, 0,     0],
                           [- sin r, 0, cos r, 0],
                           [0,       0,     0, 1]]

rotationZ :: Double -> UMatrix
rotationZ r = makeUMatrix [[cos r, - sin r, 0, 0],
                           [sin r,   cos r, 0, 0],
                           [0,           0, 1, 0],
                           [0,           0, 0, 1]]

shearing :: Double -> Double -> Double -> Double -> Double -> Double -> UMatrix
shearing xy xz yx yz zx zy = makeUMatrix [[1 , xy, xz, 0],
                                          [yx, 1 , yz, 0],
                                          [zx, zy, 1 , 0],
                                          [0 , 0 , 0 , 1]]

transform :: [UMatrix] -> UMatrix
transform ms = fold (reverse ms)
