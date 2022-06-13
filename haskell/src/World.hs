{-# LANGUAGE NamedFieldPuns #-}

module World
  ( World(..)
  , defaultWorld
  , intersectWorld
  , shadeHit
  , colorAt
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
                                    , Spheres.transform = identityV
                                    , Spheres.material  = Material
                                      { color     = (Color (Red 0.8) (Green 1) (Blue 0.6))
                                      , ambient   = 0.1
                                      , diffuse   = 0.7
                                      , specular  = 0.2
                                      , shininess = 200 }}
                   defaultSphere2 = Sphere
                                    { Spheres.id        = 2
                                    , radius            = 1.0
                                    , Spheres.transform = scaling 0.5 0.5 0.5
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
  = DL.sort $ concatMap (\obj -> Intersections.intersect obj r) objects

shadeHit :: World -> C.Computation -> Color
shadeHit world c = Lights.lighting
                   (Spheres.material (C.object c))
                   (light world)
                   (C.point c)
                   (C.eyev c)
                   (C.normalv c)

colorAt :: World -> Ray -> Color
colorAt w r = let is = intersectWorld w r
              in Color (Red 0) (Green 0) (Blue 0)

