module SceneChapter14
  ( renderSceneChapter14
  ) where

import Transformations as T
import Materials as M
import Tuples as T
import World
import Lights
import Camera as C
import Matrices
import Shapes as S
import Types

----------------------------------------
-- The Camera
----------------------------------------
camera = (makeCamera 1200 600 1.152)
         { C.transform = viewTransform
                         (T.point (-2.6) 1.5 (-3.9))
                         (T.point (-0.6) 1 (-0.8))
                         (T.point 0 1 0) }

----------------------------------------
-- light sources
----------------------------------------
lightSource = pointLight
              (T.point (-4.9) 4.9 (-1))
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
  Plane { Types.id        = 1
        , Types.transform = rotationY 0.31415
        , Types.material  =
            defaultMaterial
            { materialPattern = Just (checkersPattern
                                       (Color (Red 0.35) (Green 0.35) (Blue 0.35))
                                       (Color (Red 0.65) (Green 0.65) (Blue 0.65)))
            , specular        = 0
            , reflective      = 0.4 }
        , Types.parent    = Nothing }

ceilingPlane =
  Plane { Types.id        = 2
        , Types.transform = translation 0 5 0
        , Types.material  = defaultMaterial
                            { color      = Color (Red 0.8) (Green 0.8) (Blue 0.8)
                            , ambient    = 0.3
                            , specular   = 0 }
        , Types.parent    = Nothing }

westWall =  Plane { Types.id        = 3
                  , Types.transform = T.transform
                                      [ rotationY (pi/2)
                                      , rotationZ (pi/2)
                                      , translation (-5) 0 0]
                  , Types.material  = wallMaterial
                  , Types.parent    = Nothing }

eastWall =  Plane { Types.id        = 4
                  , Types.transform = T.transform [ rotationY (pi/2)
                                                  , rotationZ (pi/2)
                                                  , translation 5 0 0]
                  , Types.material  = wallMaterial
                  , Types.parent    = Nothing }

northWall =  Plane { Types.id = 5
                   , Types.transform = T.transform
                                       [ rotationX (pi/2)
                                       , translation 0 0 5]
                   , Types.material  = wallMaterial
                   , Types.parent    = Nothing }

southWall =  Plane { Types.id = 6
                   , Types.transform = T.transform
                                       [ rotationX (pi/2)
                                       , translation 0 0 (-5)]
                   , Types.material  = wallMaterial
                   , Types.parent    = Nothing }

----------------------------------------
-- Background balls
----------------------------------------

ball1 = Sphere { Types.id         = 7
               , radius           = 1.0
               , Types.transform  = T.transform
                                    [ scaling 0.4 0.4 0.4
                                    , translation 4.6 0.4 1]
               , Types.material   = defaultMaterial
                                    { color     = Color (Red 0.8) (Green 0.5) (Blue 0.3)
                                    , shininess = 50 }
               , Types.parent    = Nothing }

ball2 = Sphere { Types.id        = 8
               , radius          = 1.0
               , Types.transform = T.transform
                                   [ scaling 0.3 0.3 0.3
                                   , translation 4.7 0.3 0.4]
               , Types.material  = defaultMaterial
                                   { color     = Color (Red 0.9) (Green 0.4) (Blue 0.5)
                                   , shininess = 50 }
               , Types.parent    = Nothing }

ball3 = Sphere { Types.id        = 9
               , radius          = 1.0
               , Types.transform = T.transform
                                   [ scaling 0.5 0.5 0.5
                                   , translation (-1) 0.5 4.5]
               , Types.material  = defaultMaterial
                                   { color     = Color (Red 0.4) (Green 0.9) (Blue 0.6)
                                   , shininess = 50 }
               , Types.parent    = Nothing }

ball4 = Sphere { Types.id        = 10
               , radius          = 1.0
               , Types.transform = T.transform
                                   [ scaling 0.3 0.3 0.3
                                   , translation (-1.7) 0.3 4.7]
               , Types.material  = defaultMaterial
                                   { color     = Color (Red 0.4) (Green 0.6) (Blue 0.9)
                                   , shininess = 50 }
               , Types.parent    = Nothing }

----------------------------------------
-- Foreground balls
----------------------------------------

redSphere =
  Sphere { Types.id        = 11
         , radius          = 1.0
         , Types.transform = translation (-0.6) 1 0.6
         , Types.material  = defaultMaterial
                             { color     = Color (Red 1) (Green 0.3) (Blue 0.2)
                             , specular  = 0.4
                             , shininess = 5 }
         , Types.parent    = Nothing }

blueGlassSphere =
  Sphere { Types.id        = 12
         , radius          = 1.0
         , Types.transform = T.transform
                             [ scaling 0.7 0.7 0.7
                             , translation 0.6 0.7 (-0.6)]
         , Types.material  = defaultMaterial
                             { color           = Color (Red 0) (Green 0) (Blue 0.2)
                             , ambient         = 0
                             , diffuse         = 0.4
                             , specular        = 0.9
                             , shininess       = 300
                             , reflective      = 0.9
                             , transparency    = 0.9
                             , refractiveIndex = 1.5 }
         , Types.parent    = Nothing }

greenGlassSphere =
  Sphere { Types.id        = 13
         , radius          = 1.0
         , Types.transform = T.transform
                             [ scaling 0.5 0.5 0.5
                             , translation (-0.7) 0.5 (-0.8)]
         , Types.material  = defaultMaterial
                             { color           = Color (Red 0) (Green 0.2) (Blue 0)
                             , ambient         = 0
                             , diffuse         = 0.4
                             , specular        = 0.9
                             , shininess       = 300
                             , reflective      = 0.9
                             , transparency    = 0.9
                             , refractiveIndex = 1.5 }
         , Types.parent    = Nothing }

backgroundGroup1 =
  let (g, _) = addChildren (defaultGroup 14) [ball1, ball2]
  in g

backgroundGroup2 =
  let (g, _) = addChildren (defaultGroup 15) [ball3, ball4]
  in g

foregroundGroup =
  let (g, _) = addChildren (defaultGroup 16) [redSphere, blueGlassSphere, greenGlassSphere]
  in g

----------------------------------------
renderSceneChapter14 =
  render
  camera
  (World
   [floorPlane, ceilingPlane, westWall, eastWall, northWall, southWall
   , backgroundGroup1
   , backgroundGroup2
   , foregroundGroup]
   lightSource)

