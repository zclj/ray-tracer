module Transformations
  ( translation
  , scaling
  , rotationX
  , rotationY
  , rotationZ
  , shearing
  , transform
  , viewTransform
  ) where

import Data.Foldable
import Matrices
import Tuples

translation :: Double -> Double -> Double -> Matrix
translation x y z = makeMatrix [[1, 0, 0, x],
                                [0, 1, 0, y],
                                [0, 0, 1, z],
                                [0, 0, 0, 1]]

scaling :: Double -> Double -> Double -> Matrix
scaling x y z = makeMatrix [[x, 0, 0, 0],
                            [0, y, 0, 0],
                            [0, 0, z, 0],
                            [0, 0, 0, 1]]

rotationX :: Double -> Matrix
rotationX r = makeMatrix [[1, 0,     0,       0],
                          [0, cos r, - sin r, 0],
                          [0, sin r, cos r,   0],
                          [0, 0,     0,       1]]

rotationY :: Double -> Matrix
rotationY r = makeMatrix [[cos r,   0, sin r, 0],
                          [0,       1, 0,     0],
                          [- sin r, 0, cos r, 0],
                          [0,       0,     0, 1]]

rotationZ :: Double -> Matrix
rotationZ r = makeMatrix [[cos r, - sin r, 0, 0],
                          [sin r,   cos r, 0, 0],
                          [0,           0, 1, 0],
                          [0,           0, 0, 1]]

shearing :: Double -> Double -> Double -> Double -> Double -> Double -> Matrix
shearing xy xz yx yz zx zy = makeMatrix [[1 , xy, xz, 0],
                                         [yx, 1 , yz, 0],
                                         [zx, zy, 1 , 0],
                                         [0 , 0 , 0 , 1]]

transform :: [Matrix] -> Matrix
transform ms = fold (reverse ms)

viewTransform :: Tuple -> Tuple -> Tuple -> Matrix
viewTransform from to up =
  let forward     = norm (to `sub` from)
      upn         = norm up
      left        = forward `cross` upn
      trueUp      = left `cross` forward
      orientation = makeMatrix
                    [[x left       , y left       , z left       , 0]
                    ,[x trueUp     , y trueUp     , z trueUp     , 0]
                    ,[- (x forward), - (y forward), - (z forward), 0]
                    ,[0              , 0              , 0              , 1]]
  in orientation `Matrices.mul` translation (- (x from)) (- (y from)) (- (z from))
