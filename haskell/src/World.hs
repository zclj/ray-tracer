module World
  ( World(..)
  , defaultWorld
  )where

import Spheres
import Materials
import Matrices
import Transformations
import Tuples
import Lights

data World = World { objects :: [Sphere]
                   , light   :: Light}

defaultWorld :: World
defaultWorld = let defaultSphere1 = Sphere
                                    { Spheres.id        = 1
                                    , radius            = 1.0
                                    , Spheres.transform = identityV
                                    , Spheres.material  = Material
                                      { color     = (Color (Red 0.8) (Green 1) (Blue 0.6))
                                      , ambient   = 0
                                      , diffuse   = 0.7
                                      , specular  = 0.2
                                      , shininess = 0 }}
                   defaultSphere2 = Sphere
                                    { Spheres.id        = 2
                                    , radius            = 1.0
                                    , Spheres.transform = scaling 0.5 0.5 0.5
                                    , Spheres.material  = Material
                                      { color     = (Color (Red 0.8) (Green 1) (Blue 0.6))
                                      , ambient   = 0
                                      , diffuse   = 0.7
                                      , specular  = 0.2
                                      , shininess = 0 }}
                   defaultLight   = pointLight
                                    (point (-10) (-10) (-10))
                                    (Color (Red 1) (Green 1) (Blue 1))
               in World [defaultSphere1, defaultSphere2] defaultLight
