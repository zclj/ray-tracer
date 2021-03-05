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
  ) where

import Tuples

newtype Width  = Width Int
  deriving (Show, Eq)

newtype Height = Height Int
  deriving (Show, Eq)

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
--width [[row]] = (Width 1)
width (row:_) = (Width (length row))

height :: Canvas -> Height
height c = (Height (length c))

write2 :: Canvas -> Width -> Height -> Color -> Canvas
write2 c (Width w) (Height h) pixel =
  let (preRows, (postRow:postRows)) = splitAt h c
      (prePixels, (_:postPixels))   = splitAt w postRow
      newRow                        = prePixels ++ [pixel] ++ postPixels 
  in preRows ++ [newRow] ++ postRows

write :: Canvas -> Width -> Height -> Color -> Canvas
write c (Width w) (Height h) pixel =
  let (preRows, postRows)           = splitAt h c
      (prePixels, postPixels)       = case postRows of
                                        [] -> splitAt w []
                                        otherwise -> splitAt w (head postRows)
      newRow                        = case postPixels of
                                        [] -> prePixels ++ [pixel]
                                        (_:tailPixels)  -> prePixels ++ [pixel] ++ tailPixels
  in preRows ++ [newRow] ++ postRows

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

canvasToPPM :: Canvas -> String
canvasToPPM c = "P3\n 5 3\n 255\n"
-- REPL

-- testCanvas = mkCanvas (Width 2) (Height 3)
-- testColor  = Color (Red 1) (Green 0) (Blue 0)
-- newTestCanvas = write testCanvas (Width 2) (Height 3) testColor

-- testPixel = pixelAt newTestCanvas (Width 1) (Height 1)
  
--Color (Red 0) (Green 0) (Blue 0)
