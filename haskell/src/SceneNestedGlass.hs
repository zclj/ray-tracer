module SceneNestedGlass
  ( renderNestedGlass
  ) where

import Transformations as T
import Materials as M
import Tuples
import World
import Lights
import Camera as C
import Matrices
import Patterns
import Shapes

----------------------------------------
-- The Camera
----------------------------------------
camera = (makeCamera 600 600 0.45)
         { C.transform = viewTransform
                         (point 0 0 (-5))
                         (point 0 0 0)
                         (point 0 1 0) }

----------------------------------------
-- light sources
----------------------------------------
lightSource = pointLight
              (point 2 10 (-5))
              (Color (Red 0.9) (Green 0.9) (Blue 0.9))


-- wall
wall =
  APlane { Shapes.id = 1
         , ashapeTransform = T.transform [ rotationX (pi/2)
                                         , translation 0 0 10]
         , ashapeMaterial  =
           defaultMaterial
           { materialPattern = Just (checkersPattern
                                      (Color (Red 0.15) (Green 0.15) (Blue 0.15))
                                      (Color (Red 0.85) (Green 0.85) (Blue 0.85)))
           , ambient  = 0.8
           , specular = 0
           , diffuse  = 0.2 }}

-- glass ball
glassBall = ASphere { Shapes.id        = 2
                    , ashapeTransform = identity
                    , asphereRadius    = 1.0
                    , ashapeMaterial  =
                      defaultMaterial
                      { color     = Color (Red 1) (Green 1) (Blue 1)
                      , ambient = 0
                      , diffuse = 0
                      , specular = 0.9
                      , shininess = 300
                      , reflective = 0.9
                      , transparency = 0.9
                      , refractiveIndex = 1.5 }}

-- hollow center
hollowBall = ASphere { Shapes.id       = 3
                     , ashapeTransform = scaling 0.5 0.5 0.5
                     , asphereRadius   = 1.0
                     , ashapeMaterial  =
                      defaultMaterial
                      { color     = Color (Red 1) (Green 1) (Blue 1)
                      , ambient = 0
                      , diffuse = 0
                      , specular = 0.9
                      , shininess = 300
                      , reflective = 0.9
                      , transparency = 0.9
                      , refractiveIndex = 1.0000034 }}

----------------------------------------
renderNestedGlass =
  render
  camera
  (World
   [wall, glassBall, hollowBall]
   lightSource)

