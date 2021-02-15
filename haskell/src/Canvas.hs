module Canvas
  ( Canvas
  , Width (..)
  , Height (..)
  , mkCanvas
  , width
  , height
  , write
  , pixelAt
  , testing
  , testList
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
write c (Width w) (Height h) pixel =
  let (preRows, postRows)     = splitAt h c
      (prePixels, postPixels) = splitAt w (head postRows)
      newRow                  = prePixels ++ [pixel] ++ (tail postPixels) 
  in preRows ++ [newRow] ++ (tail postRows)

-- REPL

testList :: [[Int]]
testList = [[1,2,3], [4,5,6], [7,8,9]]

testing :: Int -> Int -> Int -> [[Int]] -> [[Int]]
testing x y thing xs = let (ys, zs) = splitAt x xs
                           (as, bs) = splitAt y (head zs)
                           newRow   = as ++ [thing] ++ (tail bs) 
                       in ys ++ [newRow] ++ (tail zs)

pixelAt :: Canvas -> Width -> Height -> Color
pixelAt c w h = Color (Red 0) (Green 0) (Blue 0)
