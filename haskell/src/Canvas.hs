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

makeRowWithColor :: Width -> Color -> Row
makeRowWithColor (Width x) c = Row { colors = [c | _ <- [1..x]] }

makeCanvasWithColor :: Width -> Height -> Color -> Canvas
makeCanvasWithColor w h@(Height hx) c =
  Canvas { rows   = map (\ _c -> makeRowWithColor w c) [1..hx]
         , width  = w
         , height = h }

row :: Width -> Row
row w = makeRowWithColor w (Color (Red 0) (Green 0) (Blue 0))

makeCanvas :: Width -> Height -> Canvas
makeCanvas w h@(Height hx) =
  makeCanvasWithColor w h (Color (Red 0) (Green 0) (Blue 0))

widthNum :: Canvas -> Int
widthNum (Canvas { width = Width w }) = w

heightNum :: Canvas -> Int
heightNum (Canvas { height = Height h }) = h

offCanvas :: Canvas -> Width -> Height -> Bool
offCanvas c (Width w) (Height h) =
  let zeroBasedWidth = widthNum c - 1
      zeroBasedHeight = heightNum c - 1
  in w > zeroBasedWidth || h > zeroBasedHeight

replaceIn :: [a] -> a -> [a] -> [a]
replaceIn pre x []       = pre ++ [x]
replaceIn pre x (_:post) = pre ++ [x] ++ post

replaceAt :: [a] -> Int -> a -> [a]
replaceAt xs i x = replaceIn pre x post
  where (pre, post) = splitAt i xs

replaceAtBy :: [a] -> Int -> (a -> a) -> [a]
replaceAtBy xs i f = replaceAt xs i (f (head y))
  where (_, y) = splitAt i xs

replaceInRow :: Width -> Color -> Row -> Row
replaceInRow (Width i) c r = (Row (replaceAt (colors r) i c))

replaceColorInCanvas :: Canvas -> Width -> Height -> Color -> Canvas
replaceColorInCanvas c w (Height x) col = (Canvas newRows (width c) (height c))
  where newRows = (replaceAtBy (rows c) x (replaceInRow w col))

write :: Canvas -> Width -> Height -> Color -> Canvas
write c w h newColor
  | offCanvas c w h = c
  | otherwise       = replaceColorInCanvas c w h newColor

pixelAt :: Canvas -> Width -> Height -> Color
pixelAt c (Width w) (Height h) =
  let (preRows, postRows)     = splitAt h (rows c)
      (prePixels, postPixels) = case postRows of
                                  [] -> splitAt w []
                                  _  -> splitAt w (colors (head postRows))
      pixel                   = case postPixels of
                                  [] -> last prePixels
                                  _  -> head postPixels
  in pixel

