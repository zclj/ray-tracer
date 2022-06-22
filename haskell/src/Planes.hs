module Planes
  (makePlane
  )where

import Shapes
import Tuples
import Matrices
import Materials

data Plane = Plane { id             :: Int
                   , planeTransform :: VMatrix
                   , planeMaterial  :: Material }

instance Shape Plane where
  shapeTransform = planeTransform
  shapeMaterial  = planeMaterial
  shapeNormalAt  = normalAt

makePlane :: Int -> Plane
makePlane id = Plane id identityV material

normalAt :: Plane -> Tuple -> Tuple
normalAt plane worldPoint = vector 0 1 0
