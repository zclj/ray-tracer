{-# LANGUAGE NamedFieldPuns #-}

module World
  ( World(..)
  , defaultWorld
  , intersectWorld
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
import Rays
import Intersections
import Lights
import qualified Computation as C

data World = World { objects :: [Sphere]
                   , light   :: Light}
             deriving(Show)

defaultWorld :: World
defaultWorld = let defaultSphere1 = Sphere
                                    { Spheres.id        = 1
                                    , radius            = 1.0
                                    , sphereTransform   = identityV
                                    , Spheres.material  = Material
                                      { color     = Color (Red 0.8) (Green 1) (Blue 0.6)
                                      , ambient   = 0.1
                                      , diffuse   = 0.7
                                      , specular  = 0.2
                                      , shininess = 200 }}
                   defaultSphere2 = Sphere
                                    { Spheres.id        = 2
                                    , radius            = 1.0
                                    , sphereTransform   = scaling 0.5 0.5 0.5
                                    , Spheres.material  = Materials.material}
                   defaultLight   = pointLight
                                    (point (-10) 10 (-10))
                                    (Color (Red 1) (Green 1) (Blue 1))
               in World [defaultSphere1, defaultSphere2] defaultLight

{-|
  Iterate over the objects in the world, intersecting each with the given `Ray`
-}
intersectWorld :: World -> Ray -> [Intersection]
intersectWorld World{ objects } r
  = DL.sort $ concatMap (`Intersections.intersect` r) objects

shadeHit :: World -> C.Computation -> Color
shadeHit world c = Lights.lighting
                   (Spheres.material (C.object c))
                   (light world)
                   (C.point c)
                   (C.eyev c)
                   (C.normalv c)
                   (isShadowed world (C.overPoint c))

colorAt :: World -> Ray -> Color
colorAt w r = let is = intersectWorld w r
                  h  = hit is
              in case h of
                   Nothing -> Color (Red 0) (Green 0) (Blue 0)
                   Just i  -> let c = prepareComputations i r
                              in shadeHit w c

isShadowed :: World -> Tuple -> Bool
isShadowed w p = let v             = Lights.position (light w) `sub` p
                     distance      = mag v
                     direction     = norm v
                     r             = makeRay p direction
                     intersections = intersectWorld w r
                     h             = hit intersections
                 in case h of
                      Just i  -> t i < distance
                      Nothing -> False
