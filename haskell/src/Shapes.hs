module Shapes where

import Matrices
import Materials
import Tuples
import Rays
import Data.List (sort, find)

class IsShape a where
  shapeTransform :: a -> VMatrix
  shapeMaterial  :: a -> Material
  shapeNormalAt  :: a -> Tuple -> Tuple
  shapeIntersect :: a -> Ray -> [Intersection a]

data Computation a = Computation { cT         :: Double
                                 , cObject    :: a
                                 , cPoint     :: Tuple
                                 , cEyev      :: Tuple
                                 , cNormalv   :: Tuple
                                 , cInside    :: Bool
                                 , cOverPoint :: Tuple}
                   deriving(Show)

objectNormalAt :: (IsShape a) => a -> Tuple -> Tuple
objectNormalAt s worldPoint =
  let objectPoint  = inverseV (shapeTransform s) `mulTV` worldPoint
      objectNormal = shapeNormalAt s objectPoint
      worldNormal  = transposeV (inverseV (shapeTransform s)) `mulTV` objectNormal
      worldNormal' = worldNormal {w=0}
  in norm worldNormal'

prepareComputations :: (IsShape a) => Intersection a -> Ray -> Computation a
prepareComputations i r =
  let it               = intersectionT i
      po               = position r it
      obj              = intersectionObject i
      normalv          = objectNormalAt obj po
      eyev             = neg (direction r)
      (inside, normal) = if (normalv `dot` eyev) < 0
                         then (True, neg normalv)
                         else (False, normalv)
  in Computation { cT         = it
                 , cObject    = obj
                 , cPoint     = po
                 , cEyev      = eyev
                 , cNormalv   = normal
                 , cInside    = inside
                 , cOverPoint = po `add` (normal `Tuples.mul` epsilon)}


data Intersection a = Intersection
                      { intersectionT      :: Double
                      , intersectionObject :: a}
                    deriving (Show, Eq, Ord)

-- |The `hit` function returns the first non-negative intersection.
-- Intersections with a negative value are 'behind', positive 'infront'
hit :: (IsShape a, Ord a) => [Intersection a] -> Maybe (Intersection a)
hit xs = find (\(Intersection t _) -> t >= 0) $ sort xs
