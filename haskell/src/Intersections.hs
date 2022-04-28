module Intersections
  ( Intersection (..)
  , intersect
  , hit
  ) where

import Spheres
import Rays
import Tuples
import Data.List (sort, find)

data Intersection = Intersection { t      :: Double
                                 , object :: Sphere}
                  deriving (Show, Eq, Ord)

intersect :: Sphere -> Ray -> [Intersection]
intersect s r = let sphereToRay  = origin r `sub` point 0 0 0
                    a            = direction r `dot` direction r
                    b            = 2 * (direction r `dot` sphereToRay)
                    c            = (sphereToRay `dot` sphereToRay) - 1
                    discriminant = b^2 - (4 * a * c)
                in if discriminant < 0
                   then []
                   else [ Intersection (((-b) - sqrt discriminant) / (2 * a)) s
                        , Intersection (((-b) + sqrt discriminant) / (2 * a)) s]

-- |The `hit` function returns the first non-negative intersection.
-- Intersections with a negative value are 'behind', positive 'infront'
hit :: [Intersection] -> Maybe Intersection
hit xs = find (\(Intersection t _) -> t > 0) $ sort xs
