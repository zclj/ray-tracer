module SceneChapter15
  ( writeSceneChapter15
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

import PPMCanvas as PPM
import ObjFileParser

----------------------------------------
-- The Camera
----------------------------------------
camera = (makeCamera 800 400 1.152)
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
-- Read OBJ-file
----------------------------------------

writeSceneChapter15 = do
  parser <- parseObjFile "obj-files/teapot-low.obj"
  let group = objToGroup parser
      dg    = divide group 20
      g     = updateTransform dg (T.transform
                                 [ rotationZ (pi/4)
                                 , rotationX (-pi/2)
                                 , scaling 0.15 0.15 0.15
                                 , translation (1.6) (0) 1.6])
  writeFile
    "teapot-low.ppm"
    (PPM.canvasToPPMString
     (render
      camera
      (World [g
             , floorPlane
             ,ceilingPlane
             , westWall
             , eastWall
             , northWall
             , southWall]
        lightSource)))

----------------------------------------
-- renderSceneChapter15 =
--   render
--   camera
--   (World
--    [triangle 1 (T.point 0 1 0) (T.point (-1) 0 0) (T.point 1 0 0)]
--    lightSource)
