module Transformations
  ( translation
  , scaling
  , rotationX
  , rotationY
  , rotationZ
  ) where

import Matrices

translation :: Double -> Double -> Double -> Matrix Double
translation x y z = makeMatrix [[1, 0, 0, x],
                                [0, 1, 0, y],
                                [0, 0, 1, z],
                                [0, 0, 0, 1]]

scaling :: Double -> Double -> Double -> Matrix Double
scaling x y z = makeMatrix [[x, 0, 0, 0],
                            [0, y, 0, 0],
                            [0, 0, z, 0],
                            [0, 0, 0, 1]]

rotationX :: Double -> Matrix Double
rotationX r = makeMatrix [[1, 0,     0,       0],
                          [0, cos r, - sin r, 0],
                          [0, sin r, cos r,   0],
                          [0, 0,     0,       1]]

rotationY :: Double -> Matrix Double
rotationY r = makeMatrix [[cos r,   0, sin r, 0],
                          [0,       1, 0,     0],
                          [- sin r, 0, cos r, 0],
                          [0,       0,     0, 1]]

rotationZ :: Double -> Matrix Double
rotationZ r = makeMatrix [[cos r, - sin r, 0, 0],
                          [sin r,   cos r, 0, 0],
                          [0,           0, 1, 0],
                          [0,           0, 0, 1]]
