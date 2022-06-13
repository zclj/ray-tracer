module Spheres
  ( Sphere (..)
  , makeUnitSphere
  , normalAt
  , setTransform
  ) where

import Tuples
import Matrices
import Materials as M

data Sphere = Sphere { id        :: Int
                     , radius    :: Double
                     , transform :: VMatrix
                     , material  :: M.Material}
              deriving (Show, Eq, Ord)

makeUnitSphere :: Int -> Sphere
makeUnitSphere id = Sphere id 1.0 identityV M.material

normalAt :: Sphere -> Tuple -> Tuple
normalAt Sphere{transform = t} worldPoint
  = let objectPoint  = inverseV t `mulTV` worldPoint
        objectNormal = objectPoint `sub` point 0 0 0
        worldNormal  = transposeV (inverseV t) `mulTV` objectNormal
        worldNormal' = worldNormal {w=0}
    in norm worldNormal'

setTransform :: Sphere -> VMatrix -> Sphere
setTransform s m = s {transform = m}
