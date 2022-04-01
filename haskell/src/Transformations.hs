module Transformations
  ( translation
  ) where

import Matrices

translation :: Double -> Double -> Double -> Matrix Double
translation x y z = makeMatrix [[1, 0, 0, x],
                                [0, 1, 0, y],
                                [0, 0, 1, z],
                                [0, 0, 0, 1]]
