module Intersection where

import Types
import Tuples
import Rays
import Shapes
import Data.List (sortBy, find)
import Data.Ord

prepareComputations :: Intersection -> Ray -> [Intersection] -> Computation
prepareComputations i r xs =
  let it               = intersectionT i
      po               = Rays.position r it
      obj              = intersectionObject i
      normalv          = objectNormalAt obj po i
      eyev             = neg (direction r)
      (inside, normal) = if (normalv `dot` eyev) < 0
                         then (True, neg normalv)
                         else (False, normalv)
      (n1, n2)         = refractive xs [] i (0.0, 0.0)
  in Computation { t          = it
                 , object     = obj
                 , Types.point = po
                 , eyev       = eyev
                 , normalv    = normal
                 , inside     = inside
                 , overPoint  = po `add` (normal `mul` epsilon)
                 , underPoint = po `sub` (normal `mul` epsilon)
                 , reflectv   = reflect (direction r) normal
                 , n1         = n1
                 , n2         = n2 }

-- |The `hit` function returns the first non-negative intersection.
-- Intersections with a negative value are 'behind', positive 'infront'
hit :: [Intersection] -> Maybe Intersection
hit xs = find (\i -> case i of
                       (Intersection t _) -> t >= 0
                       (IntersectionUV t _ _ _) -> t >= 0)
         $ sortBy (comparing intersectionT) xs

schlick :: Computation -> Double
schlick c =
  let n      = n1 c / n2 c
      cos    = eyev c `dot` normalv c
      sin2_t = n**2 * (1.0 - cos**2)
      cos_t  = sqrt (1.0 - sin2_t)
      r0     = ((n1 c - n2 c) / (n1 c + n2 c))**2
  in if n1 c > n2 c
     then if sin2_t > 1.0
          then 1.0
          else r0 + (1.0 - r0) * ((1 - cos_t)**5)
     else r0 + (1.0 - r0) * ((1 - cos)**5)
