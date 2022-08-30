module Planes
  ( makePlane
  )where

import Shapes
import Tuples
import Matrices
import Materials
import Rays

-- data Plane = Plane { id             :: Int
--                    , planeTransform :: Matrix
--                    , planeMaterial  :: Material }
--              deriving(Show, Eq, Ord)

-- instance IsShape Plane where
--   shapeId        = Planes.id
--   shapeTransform = planeTransform
--   shapeMaterial  = planeMaterial
--   shapeNormalAt  = normalAt
--   shapeIntersect = intersect

makePlane :: Int -> AShape
makePlane id = APlane id identity defaultMaterial

-- toAShape :: Plane -> AShape
-- toAShape p = APlane (Planes.id p) (planeTransform p) (planeMaterial p)

-- normalat :: Plane -> Tuple -> Tuple
-- normalAt _ _ = vector 0 1 0

intersect :: AShape -> Ray -> [Intersection]
intersect p r = if abs(y (direction r)) < epsilon
                then []
                else let t = -y (origin r) / y (direction r)
                     in [Intersection t p]
