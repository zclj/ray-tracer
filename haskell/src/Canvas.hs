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
  Canvas { rows   = foldr (\_ canvas ->
                             makeRowWithColor w c : canvas) [] [1..hx]
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

write :: Canvas -> Width -> Height -> Color -> Canvas
write c cw@(Width w) ch@(Height h) pixel
  | offCanvas c cw ch                 = c
  | otherwise =
    let (preRows, postRows)           = splitAt h (rows c)
        (prePixels, postPixels)       = case postRows of
                                          [] -> splitAt w []
                                          otherwise ->
                                            splitAt w (colors (head postRows))
        newRow                        = case postPixels of
                                          [] -> prePixels ++ [pixel]
                                          (_:tailPixels)  ->
                                            prePixels ++ [pixel] ++ tailPixels
        trailingRows                  = case postRows of
                                          [] -> []
                                          otherwise -> (tail postRows)
    in (Canvas
        (preRows ++ [(Row {colors = newRow})] ++ trailingRows)
         (width c)
         (height c))

pixelAt :: Canvas -> Width -> Height -> Color
pixelAt c (Width w) (Height h) =
  let (preRows, postRows)     = splitAt h (rows c)
      (prePixels, postPixels) = case postRows of
                                  [] -> splitAt w []
                                  otherwise -> splitAt w (colors (head postRows))
      pixel                   = case postPixels of
                                  [] -> last prePixels
                                  otherwise -> head postPixels
  in pixel

