module Silhouette where

import Tuples
import Canvas
import Spheres
{- Cast rays on a sphere to make out its silhouette -}

rayOrigin = point 0 0 (-5)

wallZ = 10

wallSize :: Double
wallSize = 7.0

canvasPixels = 100
pixelSize = wallSize / (fromIntegral canvasPixels)

half = wallSize / 2

cast = let canvas = makeCanvas (Width canvasPixels) (Height canvasPixels)
           color  = Color (Red 1) (Green 0) (Blue 0)
           sphere = makeUnitSphere 1
       in canvas
          -- For each row of pixels in the canvas
          -- compute the world y coordinate (top = +half, bottom = -half)

          -- for each pixel in the row
            -- compute the world x coordinate (left = -half, right = half)
            -- describe the point on the wall that the ray will target
