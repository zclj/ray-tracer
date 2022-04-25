module Intersections
  ( Intersection (..)
  ) where

import Spheres
import Rays

data Intersection = Intersection { t      :: Double
                                 , object :: Sphere}
                  deriving (Show)

