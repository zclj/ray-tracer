module SceneNestedGlass
  ( renderNestedGlass
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
camera = (makeCamera 600 600 0.45)
         { C.transform = viewTransform
                         (T.point 0 0 (-5))
                         (T.point 0 0 0)
                         (T.point 0 1 0) }

----------------------------------------
-- light sources
----------------------------------------
lightSource = pointLight
              (T.point 2 10 (-5))
              (Color (Red 0.9) (Green 0.9) (Blue 0.9))


-- wall
wall =
  Plane { Types.id        = 1
        , Types.transform = T.transform [ rotationX (pi/2)
                                        , translation 0 0 10]
        , Types.material  =
            defaultMaterial
            { materialPattern = Just (checkersPattern
                                       (Color (Red 0.15) (Green 0.15) (Blue 0.15))
                                       (Color (Red 0.85) (Green 0.85) (Blue 0.85)))
            , ambient  = 0.8
            , specular = 0
            , diffuse  = 0.2 }
        , Types.parent    = Nothing}

-- glass ball
glassBall = Sphere { Types.id        = 2
                   , Types.transform = identity
                   , radius          = 1.0
                   , Types.material  =
                       defaultMaterial
                       { color           = Color (Red 1) (Green 1) (Blue 1)
                       , ambient         = 0
                       , diffuse         = 0
                       , specular        = 0.9
                       , shininess       = 300
                       , reflective      = 0.9
                       , transparency    = 0.9
                       , refractiveIndex = 1.5 }
                   , Types.parent    = Nothing}

-- hollow center
hollowBall = Sphere { Types.id        = 3
                    , Types.transform = scaling 0.5 0.5 0.5
                    , radius          = 1.0
                    , Types.material  =
                        defaultMaterial
                        { color     = Color (Red 1) (Green 1) (Blue 1)
                        , ambient         = 0
                        , diffuse         = 0
                        , specular        = 0.9
                        , shininess       = 300
                        , reflective      = 0.9
                        , transparency    = 0.9
                        , refractiveIndex = 1.0000034 }
                    , Types.parent    = Nothing}

----------------------------------------
renderNestedGlass =
  render
  camera
  (World
   [wall, glassBall, hollowBall]
   lightSource)

