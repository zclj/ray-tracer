module Camera
  ( Camera (..)
  , makeCamera
  ) where

import Matrices

data Camera = Camera { hsize       :: Int
                     , vsize       :: Int
                     , fieldOfView :: Double
                     , transform   :: VMatrix }

makeCamera :: Int -> Int -> Double -> Camera
makeCamera hs vs fov = undefined
