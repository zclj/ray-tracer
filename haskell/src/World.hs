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

import Materials
import Matrices
import Transformations
import Tuples as T
import Rays as R
import Lights
import Shapes
import Types
import Intersection

data World = World { shapes :: [Shape]
                   , light  :: Light}
               deriving(Show)

defaultWorld :: World
defaultWorld = let defaultSphere1 = (defaultSphere 1)
                                    { material  = Material
                                      { color     = Color (Red 0.8) (Green 1) (Blue 0.6)
                                      , ambient   = 0.1
                                      , diffuse   = 0.7
                                      , specular  = 0.2
                                      , shininess = 200
                                      , reflective = 0
                                      , transparency = 0
                                      , refractiveIndex = 1.0
                                      , materialPattern   = Nothing}}
                   defaultSphere2 = (defaultSphere 2)
                                    { Types.transform = scaling 0.5 0.5 0.5 }
                   defaultLight   = pointLight
                                    (T.point (-10) 10 (-10))
                                    (Color (Red 1) (Green 1) (Blue 1))
               in World
                  [defaultSphere1, defaultSphere2]
                  defaultLight

shadeHit :: World -> Computation -> Int -> Color
shadeHit world c remaining
  = let surface   = Lights.lighting
                    (material (object c))
                    (object c)
                    (light world)
                    (overPoint c)
                    (eyev c)
                    (normalv c)
                    (isShadowed world (overPoint c))
        reflected = reflectedColor world c remaining
        refracted = refractedColor world c remaining
        m         = material (object c)
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
  = let intersections = intersectShapes (shapes w) r
        theHit        = hit intersections
    in colorizeShape w r remaining theHit intersections

isShadowed :: World -> Tuple -> Bool
isShadowed w p = let v              = Lights.position (light w) `sub` p
                     distance       = mag v
                     direction      = norm v
                     r              = makeRay p direction
                     intersections  = intersectShapes (shapes w) r
                     h              = hit intersections
                 in case h of
                      Just i  -> intersectionT i < distance
                      Nothing -> False

reflectedColor :: World -> Computation -> Int -> Color
reflectedColor w pc remaining
  = let m = material (object pc)
    in if reflective m == 0 || remaining == 0
       then Color (Red 0) (Green 0) (Blue 0)
       else let reflectRay = makeRay (overPoint pc) (reflectv pc)
                color      = colorAt w reflectRay (remaining - 1)
            in color `mulCS` reflective m

refractedColor :: World -> Computation -> Int -> Color
refractedColor w pc remaining =
  let m       = material (object pc)
      n_ratio = n1 pc / n2 pc
      cos_i   = eyev pc `dot` normalv pc
      sin2_t  = n_ratio^2 * (1 - cos_i^2)
  in if transparency m == 0 || remaining == 0 || sin2_t > 1.0
     then Color (Red 0) (Green 0) (Blue 0)
     else let cos_t      = sqrt (1 - sin2_t)
              direction  = normalv pc `T.mul` (n_ratio * cos_i - cos_t)
                          `sub` (eyev pc `T.mul` n_ratio)
              refractRay = makeRay (underPoint pc) direction
          in colorAt w refractRay (remaining - 1) `mulCS`
             transparency (material (object pc))
