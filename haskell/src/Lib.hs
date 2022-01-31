module Lib
    ( runDemo
    , writeCanvas
    , projectileCanvas
    ) where

import Canvas
import PPMCanvas as PPM
import Tuples
import Projectile

runDemo = writeCanvas projectileCanvas

projectileCanvas :: String
projectileCanvas
  = let color       = Color (Red 1) (Green 0.8) (Blue 0.6)
        emptycanvas = makeCanvas (Width 900) (Height 550)
        canvas      = foldr (\(Projectile (Tuple x y _ _) _) canvas ->
                          write
                          canvas
                          (Width (floor x))
                          (Height (floor (550 - y)))
                          color)
                      emptycanvas
                      launchresult
    in PPM.canvasToPPMString canvas

writeCanvas :: String -> IO ()
writeCanvas c = writeFile "canvas.ppm" c
