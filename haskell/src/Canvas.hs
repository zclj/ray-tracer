module Canvas
  ( Canvas
  , Width (..)
  , Height (..)
  , Row (..)
  , rows
  , makeCanvas
  , makeCanvasWithColor
  , width
  , height
  , write
  , pixelAt
  ) where

import Tuples
import Utils

newtype Width  = Width Int
  deriving (Show, Eq, Ord)

newtype Height = Height Int
  deriving (Show, Eq, Ord)

data Row = Row { colors :: [Color]
               } deriving (Show, Eq)

data Canvas = Canvas { rows :: [Row]
                     , width :: Width
                     , height :: Height
                     } deriving (Show, Eq)

makeRow :: Color -> Width -> Row
makeRow c (Width x) = Row { colors = [c | _ <- [1..x]] }

makeCanvasWithColor :: Color -> Width -> Height -> Canvas
makeCanvasWithColor c w h@(Height hx) =
  Canvas { rows   = map (\ _c -> makeRow c w) [1..hx]
         , width  = w
         , height = h }

makeCanvas :: Width -> Height -> Canvas
makeCanvas w h@(Height hx) =
  makeCanvasWithColor (Color (Red 0) (Green 0) (Blue 0)) w h

offCanvas :: Canvas -> Width -> Height -> Bool
offCanvas Canvas { width = Width cw, height = Height ch } (Width w) (Height h) =
  let zeroBased x = x - 1
  in w > zeroBased cw || h > zeroBased ch

replaceInRow :: Width -> Color -> Row -> Row
replaceInRow (Width i) c Row { colors = rc } = Row $ replaceAt rc i c

replaceColorInCanvas :: Canvas -> Width -> Height -> Color -> Canvas
replaceColorInCanvas c w (Height x) col = Canvas newRows (width c) (height c)
  where newRows = replaceAtBy x (rows c) (replaceInRow w col)

write :: Canvas -> Width -> Height -> Color -> Canvas
write c w h newColor
  | offCanvas c w h = c
  | otherwise       = replaceColorInCanvas c w h newColor

pixelAt :: Canvas -> Width -> Height -> Color
pixelAt c cw@(Width w) ch@(Height h)
  | offCanvas c cw ch = error "Pixel outside Canvas"
  | otherwise = colors (rows c !! h) !! w

