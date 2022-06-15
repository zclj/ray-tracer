module Camera
  ( Camera (..)
  , makeCamera
  ) where

import Matrices

data Camera = Camera { hsize       :: Int
                     , vsize       :: Int
                     , fieldOfView :: Double
                     , transform   :: VMatrix
                     , pixelSize   :: Double}

makeCamera :: Int -> Int -> Double -> Camera
makeCamera hs vs fov =
  let halfView   = tan (fov / 2)
      aspect     = (fromIntegral hs) / (fromIntegral vs)
      (chw, chh) = if aspect >= 1
                   then (halfView, (halfView / aspect))
                   else (halfView * aspect, halfView)
      pxSize     = (chw * 2) / (fromIntegral hs)
  in Camera hs vs fov identityV pxSize

