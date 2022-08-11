module World
  ( World(..)
  , defaultWorld
  , shadeHit
  , colorAt
  , isShadowed
  , reflectedColor
  , refractedColor
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

data World = World { aShapes       :: [AShape]
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
                                      , transparency = 0
                                      , refractiveIndex = 1.0
                                      , materialPattern   = Nothing}}
                   defaultSphere2 = Sphere
                                    { Spheres.id      = 2
                                    , radius          = 1.0
                                    , sphereTransform = scaling 0.5 0.5 0.5
                                    , sphereMaterial  = Materials.material}
                   defaultLight   = pointLight
                                    (point (-10) 10 (-10))
                                    (Color (Red 1) (Green 1) (Blue 1))
               in World
                  [Spheres.toAShape defaultSphere1, Spheres.toAShape defaultSphere2]
                  defaultLight

shadeHit :: (IsShape a) => World -> Computation a -> Int -> Color
shadeHit world c remaining
  = let surface   = Lights.lighting
                    (shapeMaterial (cObject c))
                    (cObject c)
                    (light world)
                    (cOverPoint c)
                    (cEyev c)
                    (cNormalv c)
                    (isShadowed world (cOverPoint c))
        reflected = reflectedColor world c remaining
    in surface `addC` reflected

colorizeShape :: (IsShape a, Ord a) =>
  World -> Ray -> Int -> Maybe (Intersection a) -> Color
colorizeShape _ _ _ Nothing   = Color (Red 0) (Green 0) (Blue 0)
colorizeShape w r remaining (Just s) =
  shadeHit w (prepareComputations s r [s]) remaining

colorAt :: World -> Ray -> Int -> Color
colorAt w r remaining
  = let xx = intersectShapes (aShapes w) r
        yy = hit xx
    in colorizeShape w r remaining yy

isShadowed :: World -> Tuple -> Bool
isShadowed w p = let v              = Lights.position (light w) `sub` p
                     distance       = mag v
                     direction      = norm v
                     r              = makeRay p direction
                     intersections  = intersectShapes (aShapes w) r
                     h              = hit intersections
                 in case h of
                      Just i  -> intersectionT i < distance
                      Nothing -> False

reflectedColor :: (IsShape a) => World -> Computation a -> Int -> Color
reflectedColor w pc remaining
  = let m = (shapeMaterial (cObject pc))
    in if (reflective m) == 0 || remaining == 0
       then Color (Red 0) (Green 0) (Blue 0)
       else let reflectRay = makeRay (cOverPoint pc) (cReflectv pc)
                color      = colorAt w reflectRay (remaining - 1)
            in color `mulCS` (reflective m)

refractedColor :: (IsShape a) => World -> Computation a -> Int -> Color
refractedColor w pc remaining =
  let m = (shapeMaterial (cObject pc))
  in if transparency m == 0
     then Color (Red 0) (Green 0) (Blue 0)
     else Color (Red 1) (Green 1) (Blue 1)
