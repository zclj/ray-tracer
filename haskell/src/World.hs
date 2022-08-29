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
                                    , sphereTransform = identity
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

shadeHit :: World -> Computation -> Int -> Color
shadeHit world c remaining
  = let surface   = Lights.lighting
                    (ashapeMaterial (cObject c))
                    (cObject c)
                    (light world)
                    (cOverPoint c)
                    (cEyev c)
                    (cNormalv c)
                    (isShadowed world (cOverPoint c))
        reflected = reflectedColor world c remaining
        refracted = refractedColor world c remaining
        m         = ashapeMaterial (cObject c)
    in if reflective m > 0 && transparency m > 0
       then let reflectance = schlick c
            in surface                         `addC`
               (reflected `mulCS` reflectance) `addC`
               (refracted `mulCS` (1 - reflectance))
       else surface `addC` reflected `addC` refracted

colorizeShape :: World -> Ray -> Int -> Maybe Intersection -> [Intersection] -> Color
colorizeShape _ _ _ Nothing _  = Color (Red 0) (Green 0) (Blue 0)
colorizeShape w r remaining (Just s) is =
  shadeHit w (prepareComputations s r is) remaining

colorAt :: World -> Ray -> Int -> Color
colorAt w r remaining
  = let xx = intersectShapes (aShapes w) r
        yy = hit xx
    in colorizeShape w r remaining yy xx

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

reflectedColor :: World -> Computation -> Int -> Color
reflectedColor w pc remaining
  = let m = ashapeMaterial (cObject pc)
    in if reflective m == 0 || remaining == 0
       then Color (Red 0) (Green 0) (Blue 0)
       else let reflectRay = makeRay (cOverPoint pc) (cReflectv pc)
                color      = colorAt w reflectRay (remaining - 1)
            in color `mulCS` reflective m

refractedColor :: World -> Computation -> Int -> Color
refractedColor w pc remaining =
  let m       = ashapeMaterial (cObject pc)
      n_ratio = cN1 pc / cN2 pc
      cos_i   = cEyev pc `dot` cNormalv pc
      sin2_t  = n_ratio^2 * (1 - cos_i^2)
  in if transparency m == 0 || remaining == 0 || sin2_t > 1.0
     then Color (Red 0) (Green 0) (Blue 0)
     else let cos_t      = sqrt (1 - sin2_t)
              direction  = cNormalv pc `Tuples.mul` (n_ratio * cos_i - cos_t)
                          `sub` (cEyev pc `Tuples.mul` n_ratio)
              refractRay = makeRay (cUnderPoint pc) direction
          in colorAt w refractRay (remaining - 1) `mulCS`
             transparency (ashapeMaterial (cObject pc))
