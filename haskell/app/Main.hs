module Main where

import Lib

main :: IO ()
main = do runDemo "canvas.ppm" >> runDemoClock "canvasClock.ppm"
