module Spheres
  ( makeUnitSphere
  , setTransform
  , makeGlassSphere
  ) where

import Tuples
import Matrices
import Materials as M
import Shapes
import Rays as R

-- data Sphere = Sphere { id              :: Int
--                      , radius          :: Double
--                      , sphereTransform :: Matrix
--                      , sphereMaterial  :: M.Material}
--             deriving (Show, Eq, Ord)

-- instance IsShape Sphere where
--   shapeId        = Spheres.id
--   shapeTransform = sphereTransform
--   shapeMaterial  = sphereMaterial
--   shapeNormalAt  = normalAt
--   shapeIntersect = intersect

makeUnitSphere :: Int -> AShape
makeUnitSphere id = ASphere id 1.0 identity defaultMaterial

-- toAShape :: Sphere -> AShape
-- toAShape s = ASphere (Spheres.id s) (radius s) (sphereTransform s) (sphereMaterial s)

makeGlassSphere :: Int -> AShape
makeGlassSphere id =
  ASphere id 1.0 identity (defaultMaterial { transparency = 1.0, refractiveIndex = 1.5 })

-- normalAt :: Sphere -> Tuple -> Tuple
-- normalAt s objectPoint
--   = objectPoint `sub` point 0 0 0

setTransform :: AShape -> Matrix -> AShape
setTransform s m = s {ashapeTransform = m}

intersect :: AShape -> Ray -> [Intersection]
intersect s r = let sphereToRay  = origin r `sub` Tuples.point 0 0 0
                    a            = direction r `dot` direction r
                    b            = 2 * (direction r `dot` sphereToRay)
                    c            = (sphereToRay `dot` sphereToRay) - 1
                    discriminant = b^2 - (4 * a * c)
                in if discriminant < 0
                   then []
                   else [ Shapes.Intersection (((-b) - sqrt discriminant) / (2 * a)) s
                        , Shapes.Intersection (((-b) + sqrt discriminant) / (2 * a)) s]
