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

-- REPL

{-
newTestCanvas = let (Height h) = height cv1
                    w = width cv1
                    rowW = (\(Width w) -> [Color (Red 1) (Green 1) (Blue 0.1) | _ <- [1..w]])
                    x = foldr (\_ canvas -> rowW w : canvas) [] [1..h]
                in x

-}
