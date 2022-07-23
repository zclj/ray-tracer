module ScenePlanesAndSpheres
  (renderScenePlanes
  ) where

import Spheres as S
import Transformations
import Materials as M
import Tuples
import World
import Lights
import Camera as C
import Matrices
import Planes as P
import Patterns

-- Planes
p = (stripePattern
      (Color (Red 0.5) (Green 0.5) (Blue 0.5))
      (Color (Red 0.5) (Green 0.5) (Blue 1)))
p' = p { patternTransform = rotationY (pi/4) `mulV` scaling 0.1 0.1 0.1 }

floorPlane = Plane { P.id = 1
                   , planeTransform = scaling 10 0.01 10
                   , planeMaterial  = M.material
                                      { pattern  = Just p'
                                      , color    = Color (Red 0.9) (Green 0.9) (Blue 0.9)
                                      , specular = 0.5 }}

backdrop = Plane { P.id = 2
                 , planeTransform = translation 0 0 1.5 `mulV` rotationX (pi/2)
                 , planeMaterial  = M.material
                                    { color    = Color (Red 0.7) (Green 0.2) (Blue 0.2)
                                    , specular = 0.5 }}

wall = Plane { P.id = 3
             , planeTransform =  rotationY (pi/2) `mulV` rotationX (pi/2) `mulV` translation 1.5 1.5 0
             , planeMaterial  = M.material
                                { pattern  = Just p'
                                , color    = Color (Red 0.2) (Green 0.7) (Blue 0.2)
                                , specular = 0.2 }}

-- Spheres
middle = Sphere { S.id = 4
                , radius = 1.0
                , sphereTransform = translation (-0.5) 1 0.5
                , sphereMaterial  = M.material
                                    { color    = Color (Red 0.1) (Green 1) (Blue 0.5)
                                    , diffuse  = 0.7
                                    , specular = 0.3 }}

right = Sphere { S.id = 5
               , radius = 1.0
               , sphereTransform = translation 1.5 0.5 (-0.5) `mulV` scaling 0.5 0.5 0.5
               , sphereMaterial  = M.material
                                   { color    = Color (Red 0.5) (Green 1) (Blue 0.1)
                                   , diffuse  = 0.7
                              , specular = 0.3 }}

p1  = (stripePattern
       (Color (Red 0.9) (Green 0.9) (Blue 0.5))
       (Color (Red 1) (Green 0.5) (Blue 0.5)))
p1' = p1 { patternTransform = rotationZ (pi/4) `mulV` scaling 0.2 0.2 0.2 }

left = Sphere { S.id = 6
              , radius = 1.0
              , sphereTransform = translation (-1.5) 0.33 (-0.75) `mulV` scaling 0.33 0.33 0.33
              , sphereMaterial = M.material
                                 { pattern  = Just p1'
                                 , color    = Color (Red 1) (Green 0.8) (Blue 0.1)
                                 , diffuse  = 0.7
                                 , specular = 0.3 }}

world = defaultWorld { light = pointLight (point (-10) 10 (-10))
                               (Color (Red 1) (Green 1) (Blue 1)) }

camera = (makeCamera 1000 500 (pi/3)) { C.transform = viewTransform
                                                      (point 0 1.5 (-5))
                                                      (point 0 1 0)
                                                      (point 0 1 0) }

renderScenePlanes = render
                    camera
                    (world { sphereObjects = [middle, right, left]
                           , planeObjects  = [floorPlane, backdrop, wall]})

