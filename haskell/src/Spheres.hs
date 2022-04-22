module Spheres
  ( Sphere (..)
  , intersects
  , makeUnitSphere
  ) where

import Rays
import Tuples

data Sphere = Sphere { id     :: Int
                     , radius :: Double }


makeUnitSphere :: Int -> Sphere
makeUnitSphere id = Sphere id 1.0

intersects :: Sphere -> Ray -> [Double]
intersects s r = let sphereToRay  = (origin r) `sub` point 0 0 0
                     a            = (direction r) `dot` (direction r)
                     b            = 2 * ((direction r) `dot` sphereToRay)
                     c            = (sphereToRay `dot` sphereToRay) - 1
                     discriminant = b^2 - (4 * a * c)
                 in if discriminant < 0
                    then []
                    else [ ((-b) - (sqrt discriminant)) / (2 * a)
                         , ((-b) + (sqrt discriminant)) / (2 * a)]
