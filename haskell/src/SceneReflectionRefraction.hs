module SceneReflectionRefraction
  ( renderSceneReflectionRefraction
  ) where

import Transformations
import Materials as M
import Tuples as T
import World
import Lights
import Camera as C
import Matrices
import Shapes as S
import Types

-- Planes
checkers = checkersPattern
           (Color (Red 0.0) (Green 0.0) (Blue 0.0))
           (Color (Red 1) (Green 1) (Blue 1))

checkers' = checkers { patternTransform = scaling 0.1 0.1 0.1 }

floorPlane = Plane
             { Types.id        = 1
             , Types.transform = scaling 10 0.01 10
             , Types.material  = defaultMaterial
                                 { materialPattern = Just checkers'
                                 , color           = Color (Red 0.9) (Green 0.9) (Blue 0.9)
                                 , reflective      = 0.5
                                 , specular        = 0.5 }
             , Types.parent    = Nothing}

wallGrad = gradientPattern
           (Color (Red 0.9) (Green 0.4) (Blue 0.9))
           (Color (Red 0.9) (Green 0.1) (Blue 0.4))
wallGrad' = wallGrad { patternTransform = translation 3.5 1 1 `Matrices.mul` scaling 10 1 1 }

backdrop = Plane
           { Types.id        = 1
           , Types.transform = translation 0 0 1.5 `Matrices.mul` rotationX (pi/2)
           , Types.material  = defaultMaterial
                               { color    = Color (Red 0.7) (Green 0.2) (Blue 0.2)
                               , specular = 0.5
                               , reflective = 0.2
                               , materialPattern = Just wallGrad'}
           , Types.parent    = Nothing}

p = stripePattern
    (Color (Red 0.5) (Green 0.5) (Blue 0.5))
    (Color (Red 0.5) (Green 0.5) (Blue 1))
p' = p { patternTransform = rotationY (pi/4) `Matrices.mul` scaling 0.1 0.1 0.1 }

wall = Plane
        { Types.id = 3
        , Types.transform =
            rotationY (pi/2) `Matrices.mul` rotationX (pi/2) `Matrices.mul` translation 1.5 1.5 0
        , Types.material  = defaultMaterial
                            { materialPattern = Nothing
                            , color      = Color (Red 0) (Green 0) (Blue 0.2)
                            , reflective = 0.9
                            , ambient    = 0
                            , diffuse    = 0.4
                            , shininess  = 300
                            , specular   = 0.9 }
        , Types.parent    = Nothing}

-- Spheres
grad = gradientPattern
       (Color (Red 0.9) (Green 0.9) (Blue 0.9))
       (Color (Red 0.9) (Green 0.1) (Blue 0.1))
grad' = grad { patternTransform = translation 1.5 1 1 `Matrices.mul` scaling 3.5 1 1 }

middle = Sphere
         { Types.id        = 4
         , radius          = 1.0
         , Types.transform = translation (-0.5) 1 0.5
         , Types.material  = defaultMaterial
                             { color    = Color (Red 0.1) (Green 1) (Blue 0.5)
                             , diffuse  = 0.7
                             , specular = 0.3
                             , reflective = 0.1
                             , materialPattern = Just grad'}
         , Types.parent    = Nothing}

ring = ringPattern
       (Color (Red 0.8) (Green 0.4) (Blue 0.3))
       (Color (Red 0.3) (Green 0.4) (Blue 0.8))
ring' = ring { patternTransform = rotationZ (pi/6) `Matrices.mul` rotationX (pi/2) `Matrices.mul` scaling 0.2 1 0.2 }

right = Sphere
        { Types.id        = 5
        , radius          = 1.0
        , Types.transform = translation 1.5 0.5 (-0.5) `Matrices.mul` scaling 0.5 0.5 0.5
        , Types.material  = defaultMaterial
                            { color    = Color (Red 0.5) (Green 1) (Blue 0.1)
                            , diffuse  = 0.7
                            , materialPattern = Just ring'
                            , specular = 0.3 }
        , Types.parent    = Nothing}

p1  = stripePattern
      (Color (Red 0.9) (Green 0.9) (Blue 0.5))
      (Color (Red 1) (Green 0.5) (Blue 0.5))
p1' = SceneReflectionRefraction.p1
      { patternTransform = rotationZ (pi/4) `Matrices.mul` scaling 0.2 0.2 0.2 }

left = Sphere
       { Types.id        = 6
       , radius          = 1.0
       , Types.transform =
           translation (-1.5) 0.33 (-0.75) `Matrices.mul` scaling 0.33 0.33 0.33
       , Types.material  = defaultMaterial
                           { materialPattern  = Just p1'
                           , color    = Color (Red 1) (Green 0.8) (Blue 0.1)
                           , diffuse  = 0.7
                           , specular = 0.3 }
       , Types.parent    = Nothing}

----
-- Light source
----

lightSource = pointLight
              (T.point (-4.9) 4.9 (-1))
              (Color (Red 1) (Green 1) (Blue 1))

----
-- Camera
---
camera = (makeCamera 1200 1200 1.152) { C.transform = viewTransform
                                                     (T.point 0 1.5 (-5))
                                                     (T.point 0 1 0)
                                                     (T.point 0 1 0) }

----
-- World
----
world = defaultWorld { light = lightSource }

----
-- AShape test
---

fooShape = Sphere 8 1.0 identity (defaultMaterial
                                   { transparency = 1.0,
                                     diffuse = 0.1,
                                     ambient = 0.1,
                                     color = Color (Red 0.1) (Green 0) (Blue 0),
                                     specular = 1.0,
                                     shininess = 300,
                                     reflective = 1.0,
                                     refractiveIndex = 1.5 })
           Nothing
fooShape' = fooShape { Types.transform = translation (-0.5) 1 (-1.9)
                                         `Matrices.mul` scaling 0.4 0.4 0.4}
-- fooShape = ASphere 5 2.0
--            (translation (-2.5) 0.33 (-1.75) `mulV` scaling 0.33 0.33 0.33)
--            (M.material { materialPattern  = Just p1'
--                        , color = Color (Red 1) (Green 0.1) (Blue 0.1)
--                        , diffuse  = 0.7
--                        , specular = 0.3 })

renderSceneReflectionRefraction
  = render
    camera
    (world { shapes = [fooShape', middle, right, left, floorPlane, backdrop, wall]})


