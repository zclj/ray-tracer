module Canvas
  ( Canvas
  , Width (..)
  , Height (..)
  , mkCanvas
  , width
  , height
  , write
  , pixelAt
  , canvasToPPM
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

pixelToPPM :: Color -> [String]
pixelToPPM (Color (Red r) (Green g) (Blue b)) =
  (map (\x -> show (max (min 255 (ceiling (255 * x))) 0)) [r, g, b])

splitLine' :: [String] -> String -> Int -> String
splitLine' [] acc size        = (init acc)
splitLine' s@(x:xs) acc size  = let newSize = size + (length x)
                                in if newSize > 70
                                   then splitLine' s  ((init acc) ++ "\n") 0
                                   else splitLine' xs (acc ++ x ++ " ") (newSize + 1)

splitLine :: [String] -> String
splitLine s = splitLine' s "" 0

rowToPPM :: Row -> String
rowToPPM [] = []  
rowToPPM r  = let pxs = foldr (\c acc -> (pixelToPPM c) ++ acc) [] r
              in  splitLine pxs
                
canvasToPPM :: Canvas -> [String]
canvasToPPM c = let (Width w)  = width c
                    (Height h) = height c
                    header     = ["P3", show w ++ " " ++ show h, "255"]
                    rows       = (foldr (\r acc -> (rowToPPM r) : acc) [] c)
                in header ++ rows

canvasToPPMString :: Canvas -> String
canvasToPPMString c = unlines $ canvasToPPM c



-- REPL
testCanvas = mkCanvas (Width 5) (Height 3)

testRow = head testCanvas

testColor = head testRow

scalePixel (Color (Red r) (Green g) (Blue b))
  = map (\x ->(max (min 255 (ceiling (255 * x))) 0)) [r, g, b]


pixelSize [r, g, b] = 3

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
                         , b :: PPMSample
                         , totalSize :: Int }
              deriving(Show)

mkPPMPixel :: Color -> PPMPixel
mkPPMPixel (Color (Red r) (Green g) (Blue b))
  = PPMPixel rs gs bs s
  where rs = mkPPMSample r
        gs = mkPPMSample g
        bs = mkPPMSample b
        s  = (size rs) + (size gs) + (size bs)

type PPMRow = [PPMPixel]
type PPMCanvas = [PPMRow]

-- splitLine' :: [String] -> String -> Int -> String
-- splitLine' [] acc size        = (init acc)
-- splitLine' s@(x:xs) acc size  = let newSize = size + (length x)
--                                 in if newSize > 70
--                                    then splitLine' s  ((init acc) ++ "\n") 0
--                                    else splitLine' xs (acc ++ x ++ " ") (newSize + 1)

-- splitLine :: [String] -> String
-- splitLine s = splitLine' s "" 0

-- rowSize :: [[PPMSample]] -> Int
-- rowSize xs = foldr (\s acc -> acc + (size s)) 0 xs

rowToPPM2 :: Row -> PPMRow
rowToPPM2 r = let ppmSamples = map mkPPMPixel r
              in ppmSamples

splitPPMRow' :: PPMRow -> PPMCanvas -> Int -> PPMCanvas
splitPPMRow' [] acc sz = acc
splitPPMRow' r@(p:ps) acc sz  = let newSize = sz + (totalSize p)
                                in if newSize > 10
                                   then splitPPMRow' r acc 0
                                   else splitPPMRow' ps (acc ++ [[p]]) newSize

{-
  A PPMPixel row contains PPMPixels with a 'totalSize' for each pixel
-}

firstTestRow = head $ rowToPPM2 testRow

testRowPPM = (rowToPPM2 testRow)

tsFirstRow = totalSize firstTestRow

-- A PPMRow has a max size. A PPMCanvas must only contain PPMRows of max size.
-- When max size is reached, a new PPMRow must be inserted

splitIt :: PPMRow -> Bool
splitIt r = let ts = foldr (+) 0 (map totalSize r)
            in ts > 6

shouldSplit = splitIt testRowPPM               
              
preSplitRow = foldr (\r preSplit ->
                       if splitIt (r : preSplit)
                       then preSplit
                       else r : preSplit)
              [] (rowToPPM2 testRow)



--theList = foldr(\x acc -> x : acc) [] [1,2,3]
----------------------------------------  
splitPPMRow :: Row -> PPMCanvas
splitPPMRow r = let ppmRow = rowToPPM2 r
                in  splitPPMRow' ppmRow [] 0

-- The problem should be similar to taking the list [10,2,3,4,5,6,7] -> [[10,2],[3,4,5,],[6,7]] given a 'size' of 2. BUT the 'size' is calculated

orgList = [1,2,3,4,5,6,7,8,9]

-- make all elements the same size for now

elementSize x = 1

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
splitRow r n f = splitWhen (whenSumOf (> n) f) r

splitPPMRow2 ppmRow rowLength = splitRow ppmRow rowLength totalSize
  
ppmStuff = splitPPMRow2 testRowPPM 8

canvasToPPM2 :: Canvas -> PPMCanvas
canvasToPPM2 c = map rowToPPM2 c

makePPMHeader :: Width -> Height -> [String]
makePPMHeader (Width w) (Height h) = ["P3", show w ++ " " ++ show h, "255"]

ppmSampleToString :: Sample -> String
ppmSampleToString (Sample x) = show x
  
ppmPixelToString :: PPMPixel -> String
ppmPixelToString x = unwords [(ppmSampleToString (sample (r x))),
                              (ppmSampleToString (sample (g x))),
                              (ppmSampleToString (sample (b x)))]

ppmRowToString :: PPMRow -> String
ppmRowToString x = (unwords $ map ppmPixelToString x)

ppmCanvasToStrings :: PPMCanvas -> [String]
ppmCanvasToStrings c = map ppmRowToString c

canvasToPPMStrings :: Canvas -> [String]
canvasToPPMStrings c = header ++ ppmStringRows
  where header        = makePPMHeader (width c) (height c)
        ppmStringRows = ppmCanvasToStrings (canvasToPPM2 c)

-- canvasToPPM :: Canvas -> [String]
  -- = let (Width w)  = width c
  --                   (Height h) = height c
  --                   header     = ["P3", show w ++ " " ++ show h, "255"]
  --                   rows       = (foldr (\r acc -> (rowToPPM r) : acc) [] c)
  --               in header ++ rows
-- canvasToPPMString :: Canvas -> String

{-
newTestCanvas = let (Height h) = height cv1
                    w = width cv1
                    rowW = (\(Width w) -> [Color (Red 1) (Green 1) (Blue 0.1) | _ <- [1..w]])
                    x = foldr (\_ canvas -> rowW w : canvas) [] [1..h]
                in x

-}
