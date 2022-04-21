module Spheres
  ( Sphere (..)
  , intersects
  ) where

import Rays

data Sphere = Sphere { id :: Int }

intersects :: Sphere -> Ray -> [Double]
intersects s r = undefined
