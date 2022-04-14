module Clock where

import Transformations
import Tuples
import Matrices

{- Make the hour points of a clock with matrix transformations -}

origin = point 0 0 0

twelve = point 0 0 1

r = rotationY (3 * (pi/6))

three = mulT r twelve

clockWidth = 800 * (3/8)

screenOrigin = let scaled = point ((x origin) * clockWidth)
                                  (y origin)
                                  ((z origin) * clockWidth)
                   moved  = scaled `add` (point 400 400 0)
                   tilted = point (x moved) (z moved) 0
               in tilted

screenTwelve = mulT (transform [translation 400 400 0]) twelve

hours = [mulT (transform [rotationX (pi/6), translation 0 (-100) 0]) origin]

clock = [screenOrigin, screenTwelve]
