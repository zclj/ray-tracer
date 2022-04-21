module Spheres
  ( Sphere (..)
  , intersects
  , makeUnitSphere
  ) where

import Rays

data Sphere = Sphere { id     :: Int
                     , radius :: Double }


makeUnitSphere :: Int -> Sphere
makeUnitSphere id = Sphere id 1.0

-- https://en.wikipedia.org/wiki/Line%E2%80%93sphere_intersection
intersects :: Sphere -> Ray -> [Double]
intersects s r = undefined
