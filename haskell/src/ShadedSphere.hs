module ShadedSphere where

import Tuples as T
import Canvas
import Rays
import Shapes
import Materials as Mat
import Lights
{- Cast rays on a sphere with shading -}

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

processPixel :: Int -> Int -> Shape -> (Maybe Intersection, Ray)
processPixel x y shape = let worldX   = toWorldX x
                             worldY   = toWorldY y
                             position = T.point worldX worldY wallZ
                             ray      = makeRay rayOrigin (norm (position `sub` rayOrigin))
                             xs       = localIntersect shape ray
                         in (hit xs, ray)

castOnPixel :: Int -> Int -> Shape -> Color -> Light -> Color
castOnPixel x y s c l = let (hit, ray) = processPixel x y s
                        in case hit of
                             Just n -> let p      = Rays.position ray (intersectionT n)
                                           normal = localNormalAt (intersectionObject n) p
                                           eye    = neg (direction ray)
                                       in lighting (material (intersectionObject n)) (defaultSphere 1) l p eye normal False
                             Nothing -> Color (Red 0) (Green 0) (Blue 0)

castRow :: Int -> Shape -> Color -> Light -> [Color]
castRow y s c l = map (\x -> castOnPixel x y s c l) [0..(canvasPixels - 1)]

render :: Canvas
render = let emptyCanvas = makeCanvas (Width canvasPixels) (Height canvasPixels)
             color  = Color (Red 1) (Green 0) (Blue 0)
             sphere = (defaultSphere 1)
                      { material =
                          defaultMaterial { color = Color (Red 1) (Green 0.2) (Blue 1)}}
             lightPos = T.point (-10) 10 (-10)
             lightColor = Color (Red 1) (Green 1) (Blue 1)
             light = pointLight lightPos lightColor
             pixels
               = map (\y -> castRow y sphere color light) [0..(canvasPixels - 1)]
         in fromColors pixels

