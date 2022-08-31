module ScenePlanesAndSpheres
  (renderScenePlanes
  ) where

import Shapes
import Transformations
import Materials as M
import Tuples
import World
import Lights
import Camera as C
import Matrices
import Patterns

-- Planes
checkers = checkersPattern
           (Color (Red 0.0) (Green 0.0) (Blue 0.0))
           (Color (Red 1) (Green 1) (Blue 1))

checkers' = checkers { patternTransform = scaling 0.1 0.1 0.1 }

floorPlane = APlane { Shapes.id = 1
                    , ashapeTransform = scaling 10 0.01 10
                    , ashapeMaterial  = defaultMaterial
                                        { materialPattern  = Just checkers'
                                        , color    = Color (Red 0.9) (Green 0.9) (Blue 0.9)
                                        , specular = 0.5 }}

wallGrad = gradientPattern
           (Color (Red 0.9) (Green 0.4) (Blue 0.9))
           (Color (Red 0.9) (Green 0.1) (Blue 0.4))
wallGrad' = wallGrad { patternTransform = translation 3.5 1 1 `Matrices.mul` scaling 10 1 1 }

backdrop = APlane
  { Shapes.id = 2
  , ashapeTransform = translation 0 0 1.5 `Matrices.mul` rotationX (pi/2)
  , ashapeMaterial  = defaultMaterial
                      { color    = Color (Red 0.7) (Green 0.2) (Blue 0.2)
                      , specular = 0.5
                      , materialPattern = Just wallGrad'}}

p = stripePattern
    (Color (Red 0.5) (Green 0.5) (Blue 0.5))
    (Color (Red 0.5) (Green 0.5) (Blue 1))
p' = p { patternTransform = rotationY (pi/4) `Matrices.mul` scaling 0.1 0.1 0.1 }

wall = APlane
  { Shapes.id = 3
  , ashapeTransform =
      rotationY (pi/2) `Matrices.mul` rotationX (pi/2) `Matrices.mul` translation 1.5 1.5 0
  , ashapeMaterial  = defaultMaterial
                      { materialPattern  = Just p'
                      , color    = Color (Red 0.2) (Green 0.7) (Blue 0.2)
                      , specular = 0.2 }}

-- Spheres
grad = gradientPattern
       (Color (Red 0.9) (Green 0.9) (Blue 0.9))
       (Color (Red 0.9) (Green 0.1) (Blue 0.1))
grad' = grad { patternTransform = translation 1.5 1 1 `Matrices.mul` scaling 3.5 1 1 }

middle = ASphere { Shapes.id       = 4
                 , radius          = 1.0
                 , ashapeTransform = translation (-0.5) 1 0.5
                 , ashapeMaterial  = defaultMaterial
                                     { color    = Color (Red 0.1) (Green 1) (Blue 0.5)
                                     , diffuse  = 0.7
                                     , specular = 0.3
                                     , materialPattern = Just grad'}}

ring = ringPattern
       (Color (Red 0.8) (Green 0.4) (Blue 0.3))
       (Color (Red 0.3) (Green 0.4) (Blue 0.8))
ring' = ring { patternTransform = rotationZ (pi/6) `Matrices.mul` rotationX (pi/2) `Matrices.mul` scaling 0.2 1 0.2 }

right = ASphere
  { Shapes.id       = 5
  , radius          = 1.0
  , ashapeTransform = translation 1.5 0.5 (-0.5) `Matrices.mul` scaling 0.5 0.5 0.5
  , ashapeMaterial  = defaultMaterial
                      { color           = Color (Red 0.5) (Green 1) (Blue 0.1)
                      , diffuse         = 0.7
                      , materialPattern = Just ring'
                      , specular        = 0.3 }}

p1  = stripePattern
      (Color (Red 0.9) (Green 0.9) (Blue 0.5))
      (Color (Red 1) (Green 0.5) (Blue 0.5))
p1' = p1 { patternTransform = rotationZ (pi/4) `Matrices.mul` scaling 0.2 0.2 0.2 }

left = (defaultSphere 6)
       { ashapeTransform =
         translation (-1.5) 0.33 (-0.75) `Matrices.mul` scaling 0.33 0.33 0.33
       , ashapeMaterial = defaultMaterial
                          { materialPattern  = Just p1'
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
                    (world { aShapes = [middle, right, left, floorPlane, backdrop, wall] })

