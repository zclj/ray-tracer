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
  , testCanvasPPM
  , testCanvas
  , newTestCanvas
  ) where

import Tuples

newtype Width  = Width Int
  deriving (Show, Eq, Ord)

newtype Height = Height Int
  deriving (Show, Eq, Ord)

-- data Canvas = Canvas Width Height
--               deriving(Show, Eq)

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

rowToPPM :: Row -> [String]
rowToPPM row = foldr (\c acc -> acc ++ (pixelToPPM c)) [] (reverse row)

canvasToPPM :: Canvas -> [String]
canvasToPPM c = let (Width w)  = width c
                    (Height h) = height c
                    header     = ["P3", show w ++ " " ++ show h, "255"]
                in (foldr (\r acc -> acc ++ (map unwords [(rowToPPM r)])) header (reverse c))
-- REPL

testCanvas = mkCanvas (Width 2) (Height 3)
testCanvasPPM = canvasToPPM newTestCanvas
testColor  = Color (Red 1) (Green 0) (Blue 0)
-- newTestCanvas = write testCanvas (Width 2) (Height 3) testColor

cv1 = mkCanvas (Width 5) (Height 3)
c1  = Color (Red 1.5) (Green 0) (Blue 0)
c2  = Color (Red 0) (Green 0.5) (Blue 0)
c3  = Color (Red (-0.5)) (Green 0) (Blue 1)
cv2 = write cv1 (Width 0) (Height 0) c1
cv3 = write cv2 (Width 2) (Height 1) c2
newTestCanvas = write cv3 (Width 4) (Height 2) c3
ppm = unlines (take 3 (drop 3 (canvasToPPM newTestCanvas)))
-- testPixel = pixelAt newTestCanvas (Width 1) (Height 1)
  
--Color (Red 0) (Green 0) (Blue 0)
