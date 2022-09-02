module Rays
  ( makeRay
  , Ray(origin, direction)
  , position
  , Rays.transform
  ) where

import Tuples
import Matrices (Matrix (..), mulT)
import Types

makeRay :: Tuple -> Tuple -> Ray
makeRay p v
  | not (isPoint p)  = error "p must be a point"
  | not (isVector v) = error "v must be a vector"
  | otherwise        = Ray p v

position :: Ray -> Double -> Tuple
position (Ray origin direction) t = origin `add` (direction `mul` t )

transform :: Ray -> Matrix -> Ray
transform (Ray origin direction) m = Ray (m `mulT` origin) (m `mulT` direction)
