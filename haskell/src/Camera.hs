module Camera
  ( Camera (..)
  , makeCamera
  , rayForPixel
  , render
  ) where

import Control.Monad.Par

import Matrices
import Tuples
import World
import Canvas
import Rays

data Camera = Camera { hsize       :: Int
                     , vsize       :: Int
                     , fieldOfView :: Double
                     , transform   :: Matrix
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
  in Camera hs vs fov identity pxSize chw chh

rayForPixel :: Camera -> Int -> Int -> Ray
rayForPixel c px py = let xoffset   = (fromIntegral px + 0.5) * pixelSize c
                          yoffset   = (fromIntegral py + 0.5) * pixelSize c
                          worldX    = halfWidth c - xoffset
                          worldY    = halfHeight c - yoffset
                          pixel     = inverse (Camera.transform c)
                                      `mulT` point worldX worldY (-1)
                          origin    = inverse (Camera.transform c) `mulT` point 0 0 0
                          direction = norm (pixel `sub` origin)
                      in makeRay origin direction

renderPixel :: Int -> Int -> Camera -> World -> Color
renderPixel x y c w = let ray = rayForPixel c x y
                      in colorAt w ray 5

renderRow :: Int -> Camera -> World -> [Color]
renderRow y c w = map (\x -> renderPixel x y c w) [0..hsize c - 1]

render :: Camera -> World -> Canvas
render c w = runPar $ do
  pixels <- parMap (\y -> renderRow y c w) [0..vsize c - 1]
  return $ fromColors pixels
