module Spheres
  ( Sphere (..)
  , makeUnitSphere
  , normalAt
  , setTransform
  ) where

import Tuples
import Matrices
import Materials as M
import Shapes

data Sphere = Sphere { id              :: Int
                     , radius          :: Double
                     , sphereTransform :: VMatrix
                     , sphereMaterial  :: M.Material}
              deriving (Show, Eq, Ord)

instance Shape Sphere where
  shapeTransform = sphereTransform
  shapeMaterial  = sphereMaterial
  shapeNormalAt  = normalAt

makeUnitSphere :: Int -> Sphere
makeUnitSphere id = Sphere id 1.0 identityV M.material

normalAt :: Sphere -> Tuple -> Tuple
normalAt Sphere{sphereTransform = t} worldPoint
  = let objectPoint  = inverseV t `mulTV` worldPoint
        objectNormal = objectPoint `sub` point 0 0 0
        worldNormal  = transposeV (inverseV t) `mulTV` objectNormal
        worldNormal' = worldNormal {w=0}
    in norm worldNormal'

setTransform :: Sphere -> VMatrix -> Sphere
setTransform s m = s {sphereTransform = m}
