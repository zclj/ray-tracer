module World
  ( World(..)
  , defaultWorld
  , shadeHit
  , colorAt
  , isShadowed
  )where

import Data.List as DL

import Spheres
import Materials
import Matrices
import Transformations
import Tuples
import Rays as R
import Lights
import Shapes
import Planes

data World = World { sphereObjects :: [Sphere]
                   , planeObjects  :: [Plane]
                   , light         :: Light}
               deriving(Show)

defaultWorld :: World
defaultWorld = let defaultSphere1 = Sphere
                                    { Spheres.id      = 1
                                    , radius          = 1.0
                                    , sphereTransform = identityV
                                    , sphereMaterial  = Material
                                      { color     = Color (Red 0.8) (Green 1) (Blue 0.6)
                                      , ambient   = 0.1
                                      , diffuse   = 0.7
                                      , specular  = 0.2
                                      , shininess = 200
                                      , reflective = 0
                                      , materialPattern   = Nothing}}
                   defaultSphere2 = Sphere
                                    { Spheres.id      = 2
                                    , radius          = 1.0
                                    , sphereTransform = scaling 0.5 0.5 0.5
                                    , sphereMaterial  = Materials.material}
                   defaultLight   = pointLight
                                    (point (-10) 10 (-10))
                                    (Color (Red 1) (Green 1) (Blue 1))
               in World [defaultSphere1, defaultSphere2] [] defaultLight

shadeHit :: (IsShape a) => World -> Computation a -> Color
shadeHit world c = Lights.lighting
                   (shapeMaterial (cObject c))
                   (cObject c)
                   (light world)
                   (cOverPoint c)
                   (cEyev c)
                   (cNormalv c)
                   (isShadowed world (cOverPoint c))

colorizeShape :: (IsShape a, IsShape b) =>
  World -> Ray -> Maybe (Intersection a) -> Maybe (Intersection b) -> Color
colorizeShape _ _ Nothing Nothing   = Color (Red 0) (Green 0) (Blue 0)
colorizeShape w r (Just i) Nothing  = shadeHit w (prepareComputations i r)
colorizeShape w r Nothing (Just i)  = shadeHit w (prepareComputations i r)
colorizeShape w r (Just p) (Just s) = if intersectionT p > intersectionT s
                                      then shadeHit w (prepareComputations s r)
                                      else shadeHit w (prepareComputations p r)

colorAt :: World -> Ray -> Color
colorAt w r = let is = intersectShapes (sphereObjects w) r
                  ip = intersectShapes (planeObjects w) r
                  hs = hit is
                  hp = hit ip
              in colorizeShape w r hp hs

isShadowed :: World -> Tuple -> Bool
isShadowed w p = let v              = Lights.position (light w) `sub` p
                     distance       = mag v
                     direction      = norm v
                     r              = makeRay p direction
                     intersections  = intersectShapes (sphereObjects w) r
                     intersectionsP = intersectShapes (planeObjects w) r
                     h              = hit intersections
                     hp             = hit intersectionsP
                 in case h of
                      Just i  -> intersectionT i < distance
                      Nothing -> case hp of
                                   Just i  -> intersectionT i < distance
                                   Nothing -> False
