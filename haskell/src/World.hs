module World
  ( World(..)
  )where

import Spheres
import Lights

data World = World { objects :: [Sphere]
                   , light   :: Light}
