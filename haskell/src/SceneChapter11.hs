module SceneChapter11
  ( renderSceneChapter11
  ) where

import Transformations as T
import Materials as M
import Tuples
import World
import Lights
import Camera as C
import Matrices
import Patterns
import Shapes as S

----------------------------------------
-- The Camera
----------------------------------------
camera = (makeCamera 1200 600 1.152)
         { C.transform = viewTransform
                         (point (-2.6) 1.5 (-3.9))
                         (point (-0.6) 1 (-0.8))
                         (point 0 1 0) }

----------------------------------------
-- light sources
----------------------------------------
lightSource = pointLight
              (point (-4.9) 4.9 (-1))
              (Color (Red 1) (Green 1) (Blue 1))

----------------------------------------
-- constants
----------------------------------------
wallMaterial = defaultMaterial
               { materialPattern = Just (stripePattern
                                        (Color (Red 0.45) (Green 0.45) (Blue 0.45))
                                        (Color (Red 0.55) (Green 0.55) (Blue 0.55)))
                                   { patternTransform = T.transform
                                                        [ scaling 0.25 0.25 0.25
                                                        , rotationY (pi/2)] }
               , ambient    = 0
               , diffuse    = 0.4
               , specular   = 0
               , reflective = 0.3}

----------------------------------------
-- describe the elements of the scene
----------------------------------------

-- the checkered floor

floorPlane =
  APlane { S.id        = 1
         , S.transform = rotationY 0.31415
         , S.material  =
             defaultMaterial
             { materialPattern = Just (checkersPattern
                                       (Color (Red 0.35) (Green 0.35) (Blue 0.35))
                                       (Color (Red 0.65) (Green 0.65) (Blue 0.65)))
           , specular        = 0
           , reflective      = 0.4 }}

ceilingPlane =
  APlane { S.id        = 2
         , S.transform = translation 0 5 0
         , S.material  = defaultMaterial
                         { color      = Color (Red 0.8) (Green 0.8) (Blue 0.8)
                         , ambient    = 0.3
                         , specular   = 0 }}

westWall =  APlane { S.id        = 3
                   , S.transform = T.transform
                                   [ rotationY (pi/2)
                                   , rotationZ (pi/2)
                                   , translation (-5) 0 0]
                   , S.material  = wallMaterial }

eastWall =  APlane { S.id        = 4
                   , S.transform = T.transform [ rotationY (pi/2)
                                               , rotationZ (pi/2)
                                               , translation 5 0 0]
                   , S.material  = wallMaterial }

northWall =  APlane { S.id = 5
                    , S.transform = T.transform
                                    [ rotationX (pi/2)
                                    , translation 0 0 5]
                    , S.material  = wallMaterial }

southWall =  APlane { S.id = 6
                    , S.transform = T.transform
                                    [ rotationX (pi/2)
                                    , translation 0 0 (-5)]
                    , S.material  = wallMaterial }

----------------------------------------
-- Background balls
----------------------------------------

ball1 = ASphere { S.id         = 7
                , radius       = 1.0
                , S.transform  = T.transform
                                 [ scaling 0.4 0.4 0.4
                                 , translation 4.6 0.4 1]
                , S.material   = defaultMaterial
                                 { color     = Color (Red 0.8) (Green 0.5) (Blue 0.3)
                                 , shininess = 50 }}

ball2 = ASphere { S.id        = 8
                , radius      = 1.0
                , S.transform = T.transform
                                [ scaling 0.3 0.3 0.3
                                , translation 4.7 0.3 0.4]
                , S.material  = defaultMaterial
                                { color     = Color (Red 0.9) (Green 0.4) (Blue 0.5)
                                , shininess = 50 }}

ball3 = ASphere { S.id        = 9
                , radius      = 1.0
                , S.transform = T.transform
                                [ scaling 0.5 0.5 0.5
                                , translation (-1) 0.5 4.5]
                , S.material  = defaultMaterial
                                { color     = Color (Red 0.4) (Green 0.9) (Blue 0.6)
                                , shininess = 50 }}

ball4 = ASphere { S.id        = 10
                , radius      = 1.0
                , S.transform = T.transform
                                [ scaling 0.3 0.3 0.3
                                , translation (-1.7) 0.3 4.7]
                , S.material  = defaultMaterial
                                { color     = Color (Red 0.4) (Green 0.6) (Blue 0.9)
                                , shininess = 50 }}

----------------------------------------
-- Foreground balls
----------------------------------------

redSphere =
  ASphere { S.id        = 11
          , radius      = 1.0
          , S.transform = translation (-0.6) 1 0.6
          , S.material  = defaultMaterial
                          { color     = Color (Red 1) (Green 0.3) (Blue 0.2)
                          , specular  = 0.4
                          , shininess = 5 }}

blueGlassSphere =
  ASphere { S.id        = 12
          , radius      = 1.0
          , S.transform = T.transform
                          [ scaling 0.7 0.7 0.7
                          , translation 0.6 0.7 (-0.6)]
          , S.material  = defaultMaterial
                          { color           = Color (Red 0) (Green 0) (Blue 0.2)
                          , ambient         = 0
                          , diffuse         = 0.4
                          , specular        = 0.9
                          , shininess       = 300
                          , reflective      = 0.9
                          , transparency    = 0.9
                          , refractiveIndex = 1.5 }}

greenGlassSphere =
  ASphere { S.id        = 13
          , radius      = 1.0
          , S.transform = T.transform
                          [ scaling 0.5 0.5 0.5
                          , translation (-0.7) 0.5 (-0.8)]
          , S.material  = defaultMaterial
                          { color           = Color (Red 0) (Green 0.2) (Blue 0)
                          , ambient         = 0
                          , diffuse         = 0.4
                          , specular        = 0.9
                          , shininess       = 300
                          , reflective      = 0.9
                          , transparency    = 0.9
                          , refractiveIndex = 1.5 }}

----------------------------------------
renderSceneChapter11 =
  render
  camera
  (World
   [floorPlane, ceilingPlane, westWall, eastWall, northWall, southWall
   , ball1, ball2, ball3, ball4
   , redSphere, blueGlassSphere, greenGlassSphere]
   lightSource)
