module Intersections
  ( Intersection (..)
  , intersects
  ) where

import Spheres
import Rays
import Tuples

data Intersection = Intersection { t      :: Double
                                 , object :: Sphere}
                  deriving (Show)

intersects :: Sphere -> Ray -> [Intersection]
intersects s r = let sphereToRay  = (origin r) `sub` point 0 0 0
                     a            = (direction r) `dot` (direction r)
                     b            = 2 * ((direction r) `dot` sphereToRay)
                     c            = (sphereToRay `dot` sphereToRay) - 1
                     discriminant = b^2 - (4 * a * c)
                 in if discriminant < 0
                    then []
                    else [ Intersection (((-b) - (sqrt discriminant)) / (2 * a)) s
                         , Intersection (((-b) + (sqrt discriminant)) / (2 * a)) s]
