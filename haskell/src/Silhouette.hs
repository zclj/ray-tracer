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

toWorldX :: Int -> Double
toWorldX x = (-half) + (pixelSize * (fromIntegral x))

toWorldY :: Int -> Double
toWorldY y = half - (pixelSize * (fromIntegral y))

processPixel :: Int -> Int -> Sphere -> Maybe Intersection
processPixel x y shape = let worldX   = toWorldX x
                             worldY   = toWorldY y
                             position = point worldX worldY wallZ
                             ray      = makeRay rayOrigin (norm (position `sub` rayOrigin))
                             xs       = intersect shape ray
                         in hit xs

castOnPixel :: Int -> Int -> Sphere -> Color -> Canvas -> Canvas
castOnPixel x y s c canvas = let isHit = processPixel x y s
                             in case isHit of
                                  Just n -> write canvas (Width x) (Height y) c
                                  Nothing -> canvas

castRow :: Int -> Sphere -> Color -> Canvas -> Canvas
castRow y s c startingCanvas = foldr (\x canvas -> castOnPixel x y s c canvas)
                               startingCanvas
                               [0..(canvasPixels - 1)]

cast :: Canvas
cast = let emptyCanvas = makeCanvas (Width canvasPixels) (Height canvasPixels)
           color  = Color (Red 1) (Green 0) (Blue 0)
           sphere = makeUnitSphere 1
           writtenCanvas
             = foldr (\y canvas -> castRow y sphere color canvas)
               emptyCanvas
               [0..(canvasPixels - 1)]
       in writtenCanvas
       
          -- For each row of pixels in the canvas
          -- compute the world y coordinate (top = +half, bottom = -half)

          -- for each pixel in the row
            -- compute the world x coordinate (left = -half, right = half)
            -- describe the point on the wall that the ray will target
