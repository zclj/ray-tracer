module Canvas where

newtype Width  = Width Double
  deriving (Show, Eq)

newtype Height = Height Double
  deriving (Show, Eq)

data Canvas = Canvas { width :: Width
                     , height :: Height }
              deriving(Show, Eq)

row :: Int -> [[Int]]
row x = [[0] | x <- [1..x]]

canvas :: Int -> Int -> [[[Int]]]
canvas x y = (foldr (\_ canvas -> row x : canvas) [] [1..y])
