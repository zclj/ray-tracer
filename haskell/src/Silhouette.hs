module Silhouette where

import Tuples as T
import Canvas
import Rays
import Shapes
{- Cast rays on a sphere to make out its silhouette -}

rayOrigin = T.point 0 0 (-5)

wallZ = 10

wallSize :: Double
wallSize = 7.0

canvasPixels = 1000
pixelSize = wallSize / fromIntegral canvasPixels

half = wallSize / 2

toWorldX :: Int -> Double
toWorldX x = (-half) + (pixelSize * fromIntegral x)

toWorldY :: Int -> Double
toWorldY y = half - (pixelSize * fromIntegral y)

processPixel :: Int -> Int -> Shape -> Maybe Intersection
processPixel x y shape = let worldX   = toWorldX x
                             worldY   = toWorldY y
                             position = T.point worldX worldY wallZ
                             ray      = makeRay rayOrigin (norm (position `sub` rayOrigin))
                             xs       = localIntersect shape ray
                         in hit xs

castOnPixel :: Int -> Int -> Shape -> Color -> Color
castOnPixel x y s c = let isHit = processPixel x y s
                      in case isHit of
                           Just n -> c
                           Nothing -> Color (Red 0) (Green 0) (Blue 0)

castRow :: Int -> Shape -> Color -> [Color]
castRow y s c = map (\x -> castOnPixel x y s c) [0..(canvasPixels - 1)]

cast :: Canvas
cast = let emptyCanvas = makeCanvas (Width canvasPixels) (Height canvasPixels)
           color  = Color (Red 1) (Green 0) (Blue 0)
           sphere = defaultSphere 1
           pixels
             = map (\y -> castRow y sphere color) [0..(canvasPixels - 1)]
       in fromColors pixels

