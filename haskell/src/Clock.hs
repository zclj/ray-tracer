module Clock where

import Transformations
import Tuples
import Matrices

{- Make the hour points of a clock -}

origin = point 0 0 0

twelve = point 0 0 1

clockWidth = 800 * (3/8)

toScreen :: Tuple -> Tuple
toScreen p = let scaled = point
                          ((x p) * clockWidth)
                          (y p)
                          ((z p) * clockWidth)
                 moved  = scaled `add` (point 400 400 400)
                 tilted = point (x moved) (z moved) 0
             in tilted

hours = foldr (\i points -> (mulT (rotationY (i * (pi/6))) twelve) : points)  [] [1..12]

screenHours = map toScreen hours

clock = screenHours
