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
import Rays as R

data Sphere = Sphere { id              :: Int
                     , radius          :: Double
                     , sphereTransform :: VMatrix
                     , sphereMaterial  :: M.Material}
              deriving (Show, Eq, Ord)

instance Shape Sphere where
  shapeTransform = sphereTransform
  shapeMaterial  = sphereMaterial
  shapeNormalAt  = normalAt
  shapeIntersect = intersect

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

intersect :: (Shape a) => a -> Ray -> [Intersection a]
intersect s r = let r'           = R.transform r (inverseV (shapeTransform s))
                    sphereToRay  = origin r' `sub` Tuples.point 0 0 0
                    a            = direction r' `dot` direction r'
                    b            = 2 * (direction r' `dot` sphereToRay)
                    c            = (sphereToRay `dot` sphereToRay) - 1
                    discriminant = b^2 - (4 * a * c)
                in if discriminant < 0
                   then []
                   else [ Shapes.Intersection (((-b) - sqrt discriminant) / (2 * a)) s
                        , Shapes.Intersection (((-b) + sqrt discriminant) / (2 * a)) s]
