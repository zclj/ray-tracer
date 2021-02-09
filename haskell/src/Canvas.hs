module Canvas
  ( Canvas
  , Width (..)
  , Height (..)
  , mkCanvas
  , width
  , height
  , write
  , pixelAt
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
width (row:rows) = (Width (length row))

height :: Canvas -> Height
height c = (Height (length c))

write :: Canvas -> Width -> Height -> Color -> Canvas
write c w h color = c

pixelAt :: Canvas -> Width -> Height -> Color
pixelAt c w h = Color (Red 0) (Green 0) (Blue 0)
