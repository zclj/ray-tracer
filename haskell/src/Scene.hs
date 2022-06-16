module Scene where

import Spheres as S
import Transformations
import Materials as M
import Tuples
import Matrices

floor = Sphere { S.id = 1
               , radius = 1.0
               , S.transform = scaling 10 0.01 10
               , S.material = M.material { color    = Color (Red 1) (Green 0.9) (Blue 0.9)
                                         , specular = 0 }}

leftWall = Sphere { S.id = 2
                  , radius = 1.0
                  , S.transform = translation 0 0 5
                                  `mulV` rotationY (-pi/4) `mulV` rotationX (pi/2)
                                  `mulV` scaling 10 0.01 10
                  , S.material = M.material
                                 { color    = Color (Red 1) (Green 0.9) (Blue 0.9)
                                 , specular = 0 }}

rightWall = Sphere { S.id = 3
                   , radius = 1.0
                   , S.transform = translation 0 0 5
                                   `mulV` rotationY (pi/4) `mulV` rotationX (pi/2)
                                   `mulV` scaling 10 0.01 10
                   , S.material = M.material
                                  { color    = Color (Red 1) (Green 0.9) (Blue 0.9)
                                  , specular = 0 }}

middle = Sphere { S.id = 4
                , radius = 1.0
                , S.transform = translation (-0.5) 1 0.5
                , S.material = M.material
                               { color    = Color (Red 0.1) (Green 1) (Blue 0.5)
                               , diffuse  = 0.7
                               , specular = 0.3 }}
