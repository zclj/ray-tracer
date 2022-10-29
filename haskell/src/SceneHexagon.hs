module SceneHexagon
  ( renderSceneHexagon
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
-- Hexagon
----------------------------------------

hexagonCorner id = (defaultSphere id)
                   { Types.transform = T.transform
                                       [(scaling 0.25 0.25 0.25), (translation 0 0 (-1))] }

hexagonEdge id = (defaultCylinder id)
                 { minY = 0
                 , maxY = 1
                 , Types.transform = T.transform
                                     [ (scaling 0.25 1 0.25)
                                     , rotationZ (-pi/2)
                                     , rotationY (-pi/6)
                                     , translation 0 0 (-1)]}

hexagonSide :: Int -> Shape
hexagonSide id = fst (addChildren (defaultGroup id) [ hexagonCorner (id * 10)
                                                    , hexagonEdge (id * 100)])

hexagon = let g = defaultGroup 6
              g' = foldr (\n acc -> fst (addChild
                                         acc
                                         (updateTransform
                                          (hexagonSide n)
                                           (rotationY ((fromIntegral n) * (pi/3))))))
                   g
                   [0..5]
          in g'
           
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
              (T.point (-0.9) 1.9 (-1))
              (Color (Red 1) (Green 1) (Blue 1))

----------------------------------------

renderSceneHexagon =
  render
  camera
  (World
   [hexagon]
   lightSource)
