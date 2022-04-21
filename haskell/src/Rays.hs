module Rays
  ( makeRay
  , Ray(origin, direction)
  , position
  ) where

import Tuples

data Ray = Ray { origin :: Tuple
               , direction :: Tuple }
           deriving (Eq, Show)

makeRay :: Tuple -> Tuple -> Ray
makeRay p v
  | not (isPoint p)  = error "p must be a point"
  | not (isVector v) = error "v must be a vector"
  | otherwise        = Ray p v

position :: Ray -> Double -> Tuple
position (Ray origin direction) t = origin `add` (direction `mul` t )
