module Clock where

import Transformations
import Tuples
import Matrices

{- Make the hour points of a clock -}

origin = point 0 0 0

twelve = point 0 0 1

clockWidth = 800 * (3/8)

hourTransform :: Double -> Matrix
hourTransform i = transform [rotationY (i * (pi/6)),
                             scaling clockWidth 0 clockWidth,
                             translation 400 0 400,
                             rotationX ((3/2) * pi)]

hours :: [Tuple]
hours = map (\i -> mulT (hourTransform i) twelve) [1..12]

clock :: [Tuple]
clock = hours
