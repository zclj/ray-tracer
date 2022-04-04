module Transformations
  ( translation
  , scaling
  , rotationX
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
