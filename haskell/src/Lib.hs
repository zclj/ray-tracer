module Lib
    ( runDemo
    , writeCanvas
    , projectileCanvas
    , runDemoClock
    ) where

import Canvas
import PPMCanvas as PPM
import Tuples
import Projectile
import Clock

runDemo = writeCanvas projectileCanvas

runDemoClock = writeCanvas clockCanvas

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

clockCanvas :: String
clockCanvas
  = let color       = Color (Red 1) (Green 0.8) (Blue 0.6)
        emptyCanvas = makeCanvas (Width 800) (Height 800)
        canvas      = write emptyCanvas (Width (floor (x origin))) (Height (floor (y origin))) color
    in PPM.canvasToPPMString canvas

writeCanvas :: String -> IO ()
writeCanvas = writeFile "canvasClock.ppm"
