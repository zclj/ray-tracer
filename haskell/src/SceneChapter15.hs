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
camera = (makeCamera 200 100 1.152)
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
-- Read OBJ-file
----------------------------------------

writeSceneChapter15 = do
  parser <- parseObjFile "obj-files/teddy.obj"--"test.obj"
  let group = objToGroup parser
      g     = updateTransform group (translation 8.6 0.0 0.0)
  writeFile "foo.ppm" (PPM.canvasToPPMString (render camera (World [g] lightSource)))

-- writeSceneChapter15 =
--   writeFile
--   "foo.ppm"
--   (PPM.canvasToPPMString
--     (render camera (World [triangle 1 (T.point 0 1 0) (T.point (-1) 0 0) (T.point 1 0 0)] lightSource)))

----------------------------------------
renderSceneChapter15 =
  render
  camera
  (World
   [triangle 1 (T.point 0 1 0) (T.point (-1) 0 0) (T.point 1 0 0)]
   lightSource)
