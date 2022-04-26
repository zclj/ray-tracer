module Spheres
  ( Sphere (..)
  , makeUnitSphere
  ) where

import Rays
import Tuples

data Sphere = Sphere { id     :: Int
                     , radius :: Double }
              deriving (Show, Eq)


makeUnitSphere :: Int -> Sphere
makeUnitSphere id = Sphere id 1.0

