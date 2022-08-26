module Planes
  ( Plane(..)
  , makePlane
  , toAShape
  )where

import Shapes
import Tuples
import Matrices
import Materials
import Rays

data Plane = Plane { id             :: Int
                   , planeTransform :: Matrix
                   , planeMaterial  :: Material }
             deriving(Show, Eq, Ord)

instance IsShape Plane where
  shapeId        = Planes.id
  shapeTransform = planeTransform
  shapeMaterial  = planeMaterial
  shapeNormalAt  = normalAt
  shapeIntersect = intersect

makePlane :: Int -> Plane
makePlane id = Plane id identity material

toAShape :: Plane -> AShape
toAShape p = APlane (Planes.id p) (planeTransform p) (planeMaterial p)

normalAt :: Plane -> Tuple -> Tuple
normalAt _ _ = vector 0 1 0

intersect :: Plane -> Ray -> [Intersection Plane]
intersect p r = if abs(y (direction r)) < epsilon
                then []
                else let t = -y (origin r) / y (direction r)
                     in [Intersection t p]
