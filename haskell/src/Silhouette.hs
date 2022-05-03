module Silhouette where

import Tuples
import Canvas
import Spheres
import Rays
import Intersections
{- Cast rays on a sphere to make out its silhouette -}

rayOrigin = point 0 0 (-5)

wallZ = 10

wallSize :: Double
wallSize = 7.0

canvasPixels = 100
pixelSize = wallSize / (fromIntegral canvasPixels)

half = wallSize / 2

processPixel :: Double -> Double -> Sphere -> Maybe Intersection
processPixel x worldY shape = let worldX   = (-half) + (pixelSize * x)
                                  position = point worldX worldY wallZ
                                  ray = makeRay rayOrigin (norm (position `sub` rayOrigin))
                                  xs = intersect shape ray
                              in hit xs

cast :: Canvas
cast = let canvas = makeCanvas (Width canvasPixels) (Height canvasPixels)
           color  = Color (Red 1) (Green 0) (Blue 0)
           sphere = makeUnitSphere 1
           hit = processPixel 0 0 sphere
       in case hit of
            Just n -> write canvas (Width 0) (Height 0) color
            Nothing -> canvas
          -- For each row of pixels in the canvas
          -- compute the world y coordinate (top = +half, bottom = -half)

          -- for each pixel in the row
            -- compute the world x coordinate (left = -half, right = half)
            -- describe the point on the wall that the ray will target
