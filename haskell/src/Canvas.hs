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
widthNum c = let (Width w)  = width c
             in  w

heightNum :: Canvas -> Int
heightNum c = let (Height h)  = height c
              in  h

offCanvas :: Canvas -> Width -> Height -> Bool
offCanvas c (Width w) (Height h) =
  let zeroBasedWidth = widthNum c - 1
      zeroBasedHeight = heightNum c - 1
  in w > zeroBasedWidth || h > zeroBasedHeight

splitRow :: Row -> Width -> ([Color], [Color])
splitRow (Row []) w = ([], [])
splitRow r (Width w) = splitAt w (colors r)

splitCanvas :: Canvas -> Height -> ([Row], [Row])
splitCanvas c (Height h) = splitAt h (rows c)

replaceColor :: [Color] -> Color -> [Color] -> [Color]
replaceColor pre p []       = pre ++ [p]
replaceColor pre p (_:post) = pre ++ [p] ++ post

replaceRow :: [Row] -> Row -> [Row] -> [Row]
replaceRow pre r [] = pre ++ [r]
replaceRow pre r (_:post) = pre ++ [r] ++ post

write :: Canvas -> Width -> Height -> Color -> Canvas
write c cw@(Width w) ch@(Height h) newPixel
  | offCanvas c cw ch                 = c
  | otherwise =
    let (preRows, postRows)           = splitCanvas c ch
        (prePixels, postPixels)       = splitRow (head postRows) cw
        newRow                        = replaceColor prePixels newPixel postPixels
    in
      Canvas
        (replaceRow preRows (Row newRow) postRows)
        (width c)
        (height c)

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

