module PPMCanvas
  ( canvasToPPMString
  , canvasToPPMStrings
  ) where

import Canvas
import Tuples
import Utils

data Sample = Sample Int
  deriving (Show)

data PPMSample = PPMSample { sample :: Sample
                           , size   :: Int }
               deriving (Show)

makeSample :: Double -> Sample
makeSample x = Sample (max (min 255 (ceiling (255 * x))) 0)

sampleSize :: Sample -> Int
sampleSize (Sample x)
  | x < 10    = 1
  | x < 100   = 2
  | otherwise = 3
  
makePPMSample :: Double -> PPMSample
makePPMSample x = let sample = makeSample x
                      size   = sampleSize sample
                in PPMSample sample size

data PPMPixel = PPMPixel { r :: PPMSample
                         , g :: PPMSample
                         , b :: PPMSample}
              deriving(Show)

makePPMPixel :: Color -> PPMPixel
makePPMPixel (Color (Red r) (Green g) (Blue b))
  = PPMPixel rs gs bs
  where rs = makePPMSample r
        gs = makePPMSample g
        bs = makePPMSample b
        
type PPMRow = [PPMPixel]
type PPMSamplesRow = [PPMSample]
type PPMCanvas = [PPMSamplesRow]

rowToPPM :: Row -> PPMRow
rowToPPM r = let ppmSamples = map makePPMPixel r
             in ppmSamples
                
splitPPMRow :: PPMRow -> Int -> PPMCanvas
splitPPMRow ppmRow rowLength = splitList (ppmRowToPPMSampleRow ppmRow) rowLength size

ppmPixelToSampleList :: PPMPixel -> [PPMSample]
ppmPixelToSampleList (PPMPixel r g b) = [r, g, b]

ppmRowToPPMSampleRow :: PPMRow -> PPMSamplesRow
ppmRowToPPMSampleRow r = concat $ map ppmPixelToSampleList r

canvasToPPM :: Canvas -> PPMCanvas
canvasToPPM c = concat $ map (\r -> splitPPMRow (rowToPPM r) 70) c

ppmSampleToString :: Sample -> String
ppmSampleToString (Sample x) = show x
  
ppmPixelToString :: PPMSample -> String
ppmPixelToString (PPMSample sample _) = ppmSampleToString sample

ppmRowToString :: PPMSamplesRow -> String
ppmRowToString x = (unwords $ map ppmPixelToString x)

ppmCanvasToStrings :: PPMCanvas -> [String]
ppmCanvasToStrings c = map ppmRowToString c

makePPMHeader :: Width -> Height -> [String]
makePPMHeader (Width w) (Height h) = ["P3", show w ++ " " ++ show h, "255"]

canvasToPPMStrings :: Canvas -> [String]
canvasToPPMStrings c = header ++ ppmStringRows
  where header        = makePPMHeader (width c) (height c)
        ppmStringRows = ppmCanvasToStrings (canvasToPPM c)

canvasToPPMString :: Canvas -> String
canvasToPPMString c = unlines $ canvasToPPMStrings c