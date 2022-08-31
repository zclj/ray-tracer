module Scene
  (renderScene
  ) where

import Shapes
import Transformations
import Materials as M
import Tuples
import World
import Lights
import Camera as C
import Matrices

floor = ASphere { Shapes.id       = 1
                , asphereRadius   = 1.0
                , ashapeTransform = scaling 10 0.01 10
                , ashapeMaterial  = defaultMaterial
                                    { color    = Color (Red 1) (Green 0.9) (Blue 0.9)
                                    , specular = 0 }}

leftWall = ASphere { Shapes.id = 2
                   , asphereRadius = 1.0
                   , ashapeTransform = translation 0 0 5
                                       `Matrices.mul` rotationY (-pi/4)
                                       `Matrices.mul` rotationX (pi/2)
                                       `Matrices.mul` scaling 10 0.01 10
                   , ashapeMaterial = defaultMaterial
                                      { color    = Color (Red 1) (Green 0.9) (Blue 0.9)
                                      , specular = 0 }}

rightWall = ASphere { Shapes.id = 3
                    , asphereRadius = 1.0
                    , ashapeTransform = translation 0 0 5
                                        `Matrices.mul` rotationY (pi/4)
                                        `Matrices.mul` rotationX (pi/2)
                                        `Matrices.mul` scaling 10 0.01 10
                    , ashapeMaterial = defaultMaterial
                                       { color    = Color (Red 1) (Green 0.9) (Blue 0.9)
                                       , specular = 0 }}

middle = ASphere { Shapes.id = 4
                 , asphereRadius = 1.0
                 , ashapeTransform = translation (-0.5) 1 0.5
                 , ashapeMaterial  = defaultMaterial
                                     { color    = Color (Red 0.1) (Green 1) (Blue 0.5)
                                     , diffuse  = 0.7
                                     , specular = 0.3 }}

right = ASphere { Shapes.id = 5
                , asphereRadius = 1.0
                , ashapeTransform = translation 1.5 0.5 (-0.5)
                                    `Matrices.mul` scaling 0.5 0.5 0.5
                , ashapeMaterial  = defaultMaterial
                                    { color    = Color (Red 0.5) (Green 1) (Blue 0.1)
                                    , diffuse  = 0.7
                                    , specular = 0.3 }}

left = ASphere { Shapes.id = 6
               , asphereRadius = 1.0
               , ashapeTransform = translation (-1.5) 0.33 (-0.75)
                                   `Matrices.mul` scaling 0.33 0.33 0.33
               , ashapeMaterial = defaultMaterial
                                  { color    = Color (Red 1) (Green 0.8) (Blue 0.1)
                                  , diffuse  = 0.7
                                  , specular = 0.3 }}

world = defaultWorld { light = pointLight (point (-10) 10 (-10))
                               (Color (Red 1) (Green 1) (Blue 1)) }

camera = (makeCamera 1000 500 (pi/3)) { C.transform = viewTransform
                                                      (point 0 1.5 (-5))
                                                      (point 0 1 0)
                                                      (point 0 1 0) }

renderScene = render
              camera
              (world { aShapes = [Scene.floor, leftWall, rightWall, middle, right, left] })
