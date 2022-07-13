module Planes
  ( Plane(..)
  , makePlane
  )where

import Shapes
import Tuples
import Matrices
import Materials
import Rays

data Plane = Plane { id             :: Int
                   , planeTransform :: VMatrix
                   , planeMaterial  :: Material }
             deriving(Show, Eq, Ord)

instance IsShape Plane where
  shapeTransform = planeTransform
  shapeMaterial  = planeMaterial
  shapeNormalAt  = normalAt
  shapeIntersect = intersect

makePlane :: Int -> Plane
makePlane id = Plane id identityV material

localNormalAt :: Tuple
localNormalAt = vector 0 1 0

normalAt :: Plane -> Tuple -> Tuple
normalAt Plane {planeTransform = t} worldPoint
  = let localPoint   = inverseV t `mulTV` worldPoint
        localNormal  = localNormalAt
        worldNormal  = transposeV (inverseV t) `mulTV` localNormal
        worldNormal' = worldNormal {w=0}
    in norm worldNormal'

intersect :: Plane -> Ray -> [Intersection Plane]
intersect p r = let r' = Rays.transform r (inverseV (shapeTransform p))
                in if abs(y (direction r')) < epsilon
                   then []
                   else let t = -(y (origin r')) / (y (direction r'))
                        in [Intersection t p]
