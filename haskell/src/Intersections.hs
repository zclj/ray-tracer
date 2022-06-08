module Intersections
  ( Intersection (..)
  , intersect
  , hit
  , prepareComputations
  ) where

import Spheres as S
import Rays as R
import Tuples
import Matrices
import qualified Computation as C
import Data.List (sort, find)

data Intersection = Intersection { t      :: Double
                                 , object :: Sphere}
                  deriving (Show, Eq, Ord)

intersect :: Sphere -> Ray -> [Intersection]
intersect s r = let r'           = R.transform r (inverseV (S.transform s))
                    sphereToRay  = origin r' `sub` Tuples.point 0 0 0
                    a            = direction r' `dot` direction r'
                    b            = 2 * (direction r' `dot` sphereToRay)
                    c            = (sphereToRay `dot` sphereToRay) - 1
                    discriminant = b^2 - (4 * a * c)
                in if discriminant < 0
                   then []
                   else [ Intersection (((-b) - sqrt discriminant) / (2 * a)) s
                        , Intersection (((-b) + sqrt discriminant) / (2 * a)) s]

-- |The `hit` function returns the first non-negative intersection.
-- Intersections with a negative value are 'behind', positive 'infront'
hit :: [Intersection] -> Maybe Intersection
hit xs = find (\(Intersection t _) -> t >= 0) $ sort xs

prepareComputations :: Intersection -> Ray -> C.Computation
prepareComputations i r =
  C.Computation { C.t       = (t i)
                , C.object  = object i
                , C.point   = position r (t i)
                , C.eyev    = neg (direction r)
                , C.normalv = normalAt (object i) (position r (t i))}
