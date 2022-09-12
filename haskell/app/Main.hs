module Main where

import Lib

main :: IO ()
main = do runDemo
            "./demos/canvas.ppm"
            -- >> runDemoClock "./demos/canvasClock.ppm"
            -- >> runDemoSilhouette "./demos/canvasSilhouette.ppm"
            -- >> runDemoShadedSphere "./demos/canvasShadedSphere.ppm"
            -- >> runDemoScene "./demos/canvasScene.ppm"
            -- >> runDemoScenePlanes "./demos/canvasScenePlanes.ppm"
            -- >> runDemoSceneReflectionRefraction "./demos/canvasSceneReflectionRefraction.ppm"
            -- >> runDemoSceneChapter11 "./demos/canvasSceneChapter11.ppm"
            >> runDemoSceneCubes "./demos/canvasSceneCubes.ppm"
            -- >> (runDemoSceneNestedGlass "./demos/canvasSceneNestedGlass.ppm")
