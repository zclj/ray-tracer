module Lib
    ( runDemo
    , demoCanvas
    , writeCanvas
    , projectileCanvas
    ) where

import Canvas
import PPMCanvas as PPM
import Tuples
import Projectile

runDemo = writeCanvas projectileCanvas

demoCanvas =  let writeRow = (\w -> [Color (Red 1) (Green 0.8) (Blue 0.6) | _ <- [1..100]])
                  coloredCanvas = foldr (\_ canvas -> writeRow w : canvas) [] [1..20]
                  ppm           = PPM.canvasToPPMString coloredCanvas
              in ppm

-- subtract projectiles y from canvas height

projectileCanvas :: String
projectileCanvas
  = let color       = Color (Red 1) (Green 0.8) (Blue 0.6)
        emptycanvas = mkCanvas (Width 900) (Height 550)
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
