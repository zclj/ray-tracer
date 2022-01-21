module Canvas
  ( Canvas
  , Width (..)
  , Height (..)
  , mkCanvas
  , width
  , height
  , write
  , pixelAt
  , canvasToPPMString
  , canvasToPPMStrings
  ) where

import Tuples

newtype Width  = Width Int
  deriving (Show, Eq, Ord)

newtype Height = Height Int
  deriving (Show, Eq, Ord)

type Row = [Color]
type Canvas = [Row]

row :: Width -> Row
row (Width x) = [Color (Red 0) (Green 0) (Blue 0) | _ <- [1..x]]

mkCanvas :: Width -> Height -> Canvas
mkCanvas w (Height h) = foldr (\_ canvas -> row w : canvas) [] [1..h]

width :: Canvas -> Width
width [] = (Width 0)
width (row:_) = (Width (length row))

height :: Canvas -> Height
height c = (Height (length c))

widthNum :: Canvas -> Int
widthNum c = let (Width w)  = width c
             in  w

heightNum :: Canvas -> Int
heightNum c = let (Width w)  = width c
              in  w

offCanvas :: Canvas -> Width -> Height -> Bool
offCanvas c (Width w) (Height h) =
  let zeroBasedWidth = ((widthNum c) - 1)
      zeroBasedHeight = ((heightNum c) - 1)
  in w > zeroBasedWidth || h > zeroBasedHeight

write :: Canvas -> Width -> Height -> Color -> Canvas
write c cw@(Width w) ch@(Height h) pixel
  | offCanvas c cw ch                 = c
  | otherwise =
    let (preRows, postRows)           = splitAt h c
        (prePixels, postPixels)       = case postRows of
                                          [] -> splitAt w []
                                          otherwise -> splitAt w (head postRows)
        newRow                        = case postPixels of
                                          [] -> prePixels ++ [pixel]
                                          (_:tailPixels)  -> prePixels ++ [pixel] ++ tailPixels
        trailingRows                  = case postRows of
                                          [] -> []
                                          otherwise -> (tail postRows)
    in preRows ++ [newRow] ++ trailingRows

pixelAt :: Canvas -> Width -> Height -> Color
pixelAt c (Width w) (Height h) =
  let (preRows, postRows)     = splitAt h c
      (prePixels, postPixels) = case postRows of
                                  [] -> splitAt w []
                                  otherwise -> splitAt w (head postRows)
      pixel                   = case postPixels of
                                  [] -> last prePixels
                                  otherwise -> head postPixels
  in pixel

data Sample = Sample Int
  deriving (Show)

mkSample :: Double -> Sample
mkSample x = Sample (max (min 255 (ceiling (255 * x))) 0)

sampleSize :: Sample -> Int
sampleSize (Sample x)
  | x < 10    = 1
  | x < 100   = 2
  | otherwise = 3

data PPMSample = PPMSample { sample :: Sample
                           , size   :: Int }
               deriving (Show)

mkPPMSample :: Double -> PPMSample
mkPPMSample x = let sample = mkSample x
                    size   = sampleSize sample
                in PPMSample sample size

data PPMPixel = PPMPixel { r :: PPMSample
                         , g :: PPMSample
                         , b :: PPMSample}
              deriving(Show)

mkPPMPixel :: Color -> PPMPixel
mkPPMPixel (Color (Red r) (Green g) (Blue b))
  = PPMPixel rs gs bs
  where rs = mkPPMSample r
        gs = mkPPMSample g
        bs = mkPPMSample b

type PPMRow = [PPMPixel]
type PPMSamplesRow = [PPMSample]
type PPMCanvas = [PPMSamplesRow]

rowToPPM :: Row -> PPMRow
rowToPPM r = let ppmSamples = map mkPPMPixel r
             in ppmSamples

addToLast :: a -> [[a]] -> [[a]]
addToLast x xs = pre ++ [post ++ [x]]
  where pre  = (init xs)
        post = (last xs)

whenSumOf :: (Num b, Ord b) => (b -> Bool) -> (a -> b) -> [a] -> Bool
whenSumOf p f l = p $ sum (map f l)

splitWhenR :: ([a] -> Bool) -> [a] -> [[a]] -> [[a]]
splitWhenR p [] acc        = acc
splitWhenR p r@(x:xs) acc  = if p (last (addToLast x acc))
                             then splitWhenR p r (acc ++ [[]])
                             else splitWhenR p xs (addToLast x acc)

splitWhen :: ([a] -> Bool) -> [a] -> [[a]]
splitWhen p xs = splitWhenR p xs [[]]

splitRow :: (Num b, Ord b) => [a] -> b -> (a -> b) -> [[a]]
splitRow r n f =
  splitWhen (\l -> (whenSumOf (> (n - (fromIntegral (length l)))) f l)) r

splitPPMRow :: PPMRow -> Int -> PPMCanvas
splitPPMRow ppmRow rowLength = splitRow (ppmRowToPPMSampleRow ppmRow) rowLength size

ppmPixelToSampleList :: PPMPixel -> [PPMSample]
ppmPixelToSampleList (PPMPixel r g b) = [r, g, b]

ppmRowToPPMSampleRow :: PPMRow -> PPMSamplesRow
ppmRowToPPMSampleRow r = concat $ map ppmPixelToSampleList r

canvasToPPM :: Canvas -> PPMCanvas
canvasToPPM c = concat $ map (\r -> splitPPMRow (rowToPPM r) 70) c

makePPMHeader :: Width -> Height -> [String]
makePPMHeader (Width w) (Height h) = ["P3", show w ++ " " ++ show h, "255"]

ppmSampleToString :: Sample -> String
ppmSampleToString (Sample x) = show x
  
ppmPixelToString :: PPMSample -> String
ppmPixelToString (PPMSample sample _) = ppmSampleToString sample

ppmRowToString :: PPMSamplesRow -> String
ppmRowToString x = (unwords $ map ppmPixelToString x)

ppmCanvasToStrings :: PPMCanvas -> [String]
ppmCanvasToStrings c = map ppmRowToString c

canvasToPPMStrings :: Canvas -> [String]
canvasToPPMStrings c = header ++ ppmStringRows
  where header        = makePPMHeader (width c) (height c)
        ppmStringRows = ppmCanvasToStrings (canvasToPPM c)

canvasToPPMString :: Canvas -> String
canvasToPPMString c = unlines $ canvasToPPMStrings c
