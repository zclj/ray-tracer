module Lib
    ( runDemo
    , writeCanvas
    , projectileCanvas
    , runDemoClock
    , runDemoSilhouette
    , runDemoShadedSphere
    ) where

import Canvas
import PPMCanvas as PPM
import Tuples
import Projectile
import Clock
import Silhouette
import ShadedSphere

runDemo fname = writeCanvas fname projectileCanvas

runDemoClock fname = writeCanvas fname clockCanvas

runDemoSilhouette fname = writeCanvas fname silhouetteCanvas

runDemoShadedSphere fname = writeCanvas fname shadedSphereCanvas

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
        canvas      = foldr (\(Tuple x y _ _) canvas ->
                               write canvas (Width (floor x)) (Height (floor y)) color)
                      emptyCanvas
                      clock
          -- write emptyCanvas (Width (floor (x origin))) (Height (floor (y origin))) color
    in PPM.canvasToPPMString canvas

silhouetteCanvas :: String
silhouetteCanvas = let c = cast
                   in PPM.canvasToPPMString c

shadedSphereCanvas :: String
shadedSphereCanvas = PPM.canvasToPPMString render

writeCanvas :: String -> String -> IO ()
writeCanvas = writeFile
