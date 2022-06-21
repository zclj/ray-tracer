module Main where

import Test.Tasty
import Test.Tasty.HUnit as HU
import TuplesSpec
import CanvasSpec
import UtilsSpec
import MatricesSpec
import TransformationsSpec
import RaysSpec
import SpheresSpec
import IntersectionsSpec
import LightsSpec
import MaterialsSpec
import WorldSpec
import CameraSpec
import ShapesSpec

-- integrate HSpec with Tasty
-- https://github.com/mitchellwrosen/tasty-hspec/issues/12
-- https://hackage.haskell.org/package/tasty-hspec-1.1.5.1/docs/Test-Tasty-Hspec.html#g:4

main = defaultMain rayTracerTests --tupleTests

rayTracerTests :: TestTree
rayTracerTests = testGroup "Ray Tracer Specs"
  [tupleTests, canvasTests, utilsTests, matricesTests, transformationTests, raysTests,
   spheresTests, intersectionsTests, lightTests, materialTests, worldTests, cameraTests
  , shapesTests]
-- main :: IO ()
-- main = putStrLn "Test suite not yet implemented"
