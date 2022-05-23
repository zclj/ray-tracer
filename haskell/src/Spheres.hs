module Spheres
  ( Sphere (..)
  , makeUnitSphere
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

