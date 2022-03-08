module PPMCanvas
  ( canvasToPPMString
  , canvasToPPMStrings
  , makePPMHeader
  ) where

import Canvas
import Tuples
import Utils
import qualified Data.Text as T

data Sample = Sample Int
  deriving (Show, Eq)

data PPMSample = PPMSample { sample :: Sample
                           , size   :: Int }
               deriving (Show, Eq)

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
rowToPPM r = let ppmSamples = map makePPMPixel (colors r)
             in ppmSamples
                
splitPPMRow :: PPMRow -> Int -> PPMCanvas
splitPPMRow ppmRow rowLength =
  splitList (ppmRowToPPMSampleRow ppmRow) rowLength size

ppmPixelToSampleList :: PPMPixel -> [PPMSample]
ppmPixelToSampleList (PPMPixel r g b) = [r, g, b]

ppmRowToPPMSampleRow :: PPMRow -> PPMSamplesRow
ppmRowToPPMSampleRow = concatMap ppmPixelToSampleList

canvasToPPM :: Canvas -> PPMCanvas
canvasToPPM c = concatMap (\r -> splitPPMRow (rowToPPM r) 70) (rows c)

ppmSampleToString :: Sample -> String
ppmSampleToString (Sample x) = show x

ppmSampleToText :: Sample -> T.Text
ppmSampleToText (Sample x) = T.pack $ show x

ppmPixelToString :: PPMSample -> String
ppmPixelToString (PPMSample sample _) = ppmSampleToString sample

ppmPixelToText :: PPMSample -> T.Text
ppmPixelToText (PPMSample sample _) = ppmSampleToText sample

ppmRowToString :: PPMSamplesRow -> String
ppmRowToString x = unwords $ map ppmPixelToString x

ppmRowToText :: PPMSamplesRow -> T.Text
ppmRowToText x = T.intercalate (T.pack " ") $ map ppmPixelToText x

-- s = map ppmPixelToText [(makePPMSample 1), (makePPMSample 0.5)]
-- s1 = T.concat $ map (\x -> x <> (T.pack " ")) s
-- prt = ppmRowToText [(makePPMSample 1), (makePPMSample 0.5)]

ppmCanvasToStrings :: PPMCanvas -> [String]
ppmCanvasToStrings = map ppmRowToString

makePPMHeader :: Width -> Height -> [String]
makePPMHeader (Width w) (Height h) = ["P3", show w ++ " " ++ show h, "255"]

makePPMHeader2 :: Width -> Height -> T.Text
makePPMHeader2 (Width w) (Height h) = (T.pack "P3\n") <> (T.pack (show w))
  <> T.singleton ' ' <> (T.pack (show h)) <> T.singleton '\n' <> (T.pack "255\n")

th = makePPMHeader2 (Width 100) (Height 100)

h = makePPMHeader (Width 100) (Height 100)
-- ["P3","100 100","255"]

canvasToPPMStrings :: Canvas -> [String]
canvasToPPMStrings c = header ++ ppmStringRows
  where header        = makePPMHeader (width c) (height c)
        ppmStringRows = ppmCanvasToStrings (canvasToPPM c)

canvasToPPMString :: Canvas -> String
canvasToPPMString c = unlines $ canvasToPPMStrings c
