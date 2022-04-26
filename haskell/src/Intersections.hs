module Intersections
  ( Intersection (..)
  , intersect
  , hit
  ) where

import Spheres
import Rays
import Tuples
import Data.List (sort)

data Intersection = Intersection { t      :: Double
                                 , object :: Sphere}
                  deriving (Show, Eq, Ord)

intersect :: Sphere -> Ray -> [Intersection]
intersect s r = let sphereToRay  = (origin r) `sub` point 0 0 0
                    a            = (direction r) `dot` (direction r)
                    b            = 2 * ((direction r) `dot` sphereToRay)
                    c            = (sphereToRay `dot` sphereToRay) - 1
                    discriminant = b^2 - (4 * a * c)
                in if discriminant < 0
                   then []
                   else [ Intersection (((-b) - (sqrt discriminant)) / (2 * a)) s
                        , Intersection (((-b) + (sqrt discriminant)) / (2 * a)) s]

hit :: [Intersection] -> Intersection
hit xs = let sorted = sort xs
         in head sorted
