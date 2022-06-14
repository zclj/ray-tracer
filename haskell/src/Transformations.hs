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

translation :: Double -> Double -> Double -> VMatrix
translation x y z = makeVMatrix [[1, 0, 0, x],
                                 [0, 1, 0, y],
                                 [0, 0, 1, z],
                                 [0, 0, 0, 1]]

scaling :: Double -> Double -> Double -> VMatrix
scaling x y z = makeVMatrix [[x, 0, 0, 0],
                             [0, y, 0, 0],
                             [0, 0, z, 0],
                             [0, 0, 0, 1]]

rotationX :: Double -> VMatrix
rotationX r = makeVMatrix [[1, 0,     0,       0],
                           [0, cos r, - sin r, 0],
                           [0, sin r, cos r,   0],
                           [0, 0,     0,       1]]

rotationY :: Double -> VMatrix
rotationY r = makeVMatrix [[cos r,   0, sin r, 0],
                           [0,       1, 0,     0],
                           [- sin r, 0, cos r, 0],
                           [0,       0,     0, 1]]

rotationZ :: Double -> VMatrix
rotationZ r = makeVMatrix [[cos r, - sin r, 0, 0],
                           [sin r,   cos r, 0, 0],
                           [0,           0, 1, 0],
                           [0,           0, 0, 1]]

shearing :: Double -> Double -> Double -> Double -> Double -> Double -> VMatrix
shearing xy xz yx yz zx zy = makeVMatrix [[1 , xy, xz, 0],
                                          [yx, 1 , yz, 0],
                                          [zx, zy, 1 , 0],
                                          [0 , 0 , 0 , 1]]

transform :: [VMatrix] -> VMatrix
transform ms = fold (reverse ms)

viewTransform :: Tuple -> Tuple -> Tuple -> VMatrix
viewTransform from to up =
  let forward     = norm (to `sub` from)
      upn         = norm up
      left        = forward `cross` upn
      trueUp      = left `cross` forward
      orientation = makeVMatrix [[(x left), (y left), (z left), 0]
                                ,[(x trueUp), (y trueUp), (z trueUp), 0]
                                ,[(- (x forward)), (- (y forward)), (- (z forward)), 0]
                                ,[0, 0, 0, 1]]
  in orientation `mulV` (translation (-(x from)) (-(y from)) (-(z from)))
