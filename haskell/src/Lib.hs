module Lib
    ( someFunc
    , demoCanvas
    , writeCanvas
    ) where

import Canvas
import Tuples

someFunc = putStrLn "someFunc"

demoCanvas =  let writeRow = (\w -> [Color (Red 1) (Green 0.8) (Blue 0.6) | _ <- [1..100]])
                  coloredCanvas = foldr (\_ canvas -> writeRow w : canvas) [] [1..20]
                  ppm           = unlines (canvasToPPM coloredCanvas)
              in ppm

writeCanvas :: String -> IO ()
writeCanvas c = writeFile "canvas.ppm" c
