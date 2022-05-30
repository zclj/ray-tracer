module Spheres
  ( Sphere (..)
  , makeUnitSphere
  , normalAt
  , setTransform
  ) where

import Rays
import Tuples
import Matrices

data Sphere = Sphere { id        :: Int
                     , radius    :: Double
                     , transform :: VMatrix}
              deriving (Show, Eq, Ord)

makeUnitSphere :: Int -> Sphere
makeUnitSphere id = Sphere id 1.0 identityV

normalAt :: Sphere -> Tuple -> Tuple
normalAt s p = norm (p `sub` (point 0 0 0))

setTransform :: Sphere -> VMatrix -> Sphere
setTransform s m = undefined
