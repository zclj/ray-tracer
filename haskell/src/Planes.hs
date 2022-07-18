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

normalAt :: Plane -> Tuple -> Tuple
normalAt _ _ = vector 0 1 0

intersect :: Plane -> Ray -> [Intersection Plane]
intersect p r = if abs(y (direction r)) < epsilon
                then []
                else let t = -y (origin r) / y (direction r)
                     in [Intersection t p]
