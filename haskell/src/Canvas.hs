module Canvas
  ( Canvas
  , Width (..)
  , Height (..)
  , makeCanvas
  , width
  , height
  ) where

import Tuples

newtype Width  = Width Int
  deriving (Show, Eq)

newtype Height = Height Int
  deriving (Show, Eq)

-- data Canvas = Canvas Width Height
--               deriving(Show, Eq)

type Canvas = [[[Int]]]

row :: Int -> [[Int]]
row x = [[0] | _ <- [1..x]]

canvas :: Int -> Int -> [[[Int]]]
canvas x y = (foldr (\_ canvas -> row x : canvas) [] [1..y])

makeCanvas :: Width -> Height -> Canvas
makeCanvas (Width w) (Height h) = canvas w h

width :: Canvas -> Width
width (row:rows) = (Width (length row))

height :: Canvas -> Height
height c = (Height (length c))
