module Scene
  (renderScene
  ) where

import Shapes as S
import Transformations
import Materials as M
import Tuples as T
import World
import Lights
import Camera as C
import Matrices
import Types

floor = Sphere { Types.id        = 1
               , radius          = 1.0
               , Types.transform = scaling 10 0.01 10
               , material        = defaultMaterial
                                   { color    = Color (Red 1) (Green 0.9) (Blue 0.9)
                                   , specular = 0 }}

leftWall = Sphere { Types.id        = 2
                  , radius          = 1.0
                  , Types.transform = translation 0 0 5
                                      `Matrices.mul` rotationY (-pi/4)
                                      `Matrices.mul` rotationX (pi/2)
                                      `Matrices.mul` scaling 10 0.01 10
                  , Types.material = defaultMaterial
                                     { color    = Color (Red 1) (Green 0.9) (Blue 0.9)
                                     , specular = 0 }}

rightWall = Sphere { Types.id        = 3
                   , radius          = 1.0
                   , Types.transform = translation 0 0 5
                                       `Matrices.mul` rotationY (pi/4)
                                       `Matrices.mul` rotationX (pi/2)
                                       `Matrices.mul` scaling 10 0.01 10
                   , Types.material = defaultMaterial
                                      { color    = Color (Red 1) (Green 0.9) (Blue 0.9)
                                      , specular = 0 }}

middle = Sphere { Types.id        = 4
                , radius          = 1.0
                , Types.transform = translation (-0.5) 1 0.5
                , Types.material  = defaultMaterial
                                    { color    = Color (Red 0.1) (Green 1) (Blue 0.5)
                                    , diffuse  = 0.7
                                    , specular = 0.3 }}

right = Sphere { Types.id        = 5
               , radius          = 1.0
               , Types.transform = translation 1.5 0.5 (-0.5)
                               `Matrices.mul` scaling 0.5 0.5 0.5
               , Types.material  = defaultMaterial
                                   { color    = Color (Red 0.5) (Green 1) (Blue 0.1)
                                   , diffuse  = 0.7
                                   , specular = 0.3 }}

left = Sphere { Types.id        = 6
              , radius          = 1.0
              , Types.transform = translation (-1.5) 0.33 (-0.75)
                                  `Matrices.mul` scaling 0.33 0.33 0.33
              , Types.material  = defaultMaterial
                                  { color    = Color (Red 1) (Green 0.8) (Blue 0.1)
                                  , diffuse  = 0.7
                                  , specular = 0.3 }}

world = defaultWorld { light = pointLight (T.point (-10) 10 (-10))
                               (Color (Red 1) (Green 1) (Blue 1)) }

camera = (makeCamera 1000 500 (pi/3)) { C.transform = viewTransform
                                                      (T.point 0 1.5 (-5))
                                                      (T.point 0 1 0)
                                                      (T.point 0 1 0) }

renderScene = render
              camera
              (world { shapes = [Scene.floor, leftWall, rightWall, middle, right, left] })
