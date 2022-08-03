module Camera
  ( Camera (..)
  , makeCamera
  , rayForPixel
  , render
  ) where

import Matrices
import Tuples
import World
import Canvas
import Rays

data Camera = Camera { hsize       :: Int
                     , vsize       :: Int
                     , fieldOfView :: Double
                     , transform   :: VMatrix
                     , pixelSize   :: Double
                     , halfWidth   :: Double
                     , halfHeight  :: Double }

makeCamera :: Int -> Int -> Double -> Camera
makeCamera hs vs fov =
  let halfView   = tan (fov / 2)
      aspect     = fromIntegral hs / fromIntegral vs
      (chw, chh) = if aspect >= 1
                   then (halfView, halfView / aspect)
                   else (halfView * aspect, halfView)
      pxSize     = (chw * 2) / fromIntegral hs
  in Camera hs vs fov identityV pxSize chw chh

rayForPixel :: Camera -> Int -> Int -> Ray
rayForPixel c px py = let xoffset   = (fromIntegral px + 0.5) * pixelSize c
                          yoffset   = (fromIntegral py + 0.5) * pixelSize c
                          worldX    = halfWidth c - xoffset
                          worldY    = halfHeight c - yoffset
                          pixel     = inverseV (Camera.transform c)
                                      `mulTV` point worldX worldY (-1)
                          origin    = inverseV (Camera.transform c) `mulTV` point 0 0 0
                          direction = norm (pixel `sub` origin)
                      in makeRay origin direction

renderPixel :: Int -> Int -> Camera -> World -> Color
renderPixel x y c w = let ray = rayForPixel c x y
                      in colorAt w ray 1

renderRow :: Int -> Camera -> World -> [Color]
renderRow y c w = map (\x -> renderPixel x y c w) [0..hsize c - 1]

render :: Camera -> World -> Canvas
render c w = let pixels = map (\y -> renderRow y c w) [0..vsize c - 1]
             in fromColors pixels
