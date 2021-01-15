module Tuples where

data Tuple = Tuple { x :: Double
                   , y :: Double
                   , z :: Double
                   , w :: Double}
             deriving (Show, Eq)

point x y z = Tuple x y z 1.0

isPoint :: Tuple -> Bool
isPoint (Tuple _ _ _ w)
  | w == 1    = True
  | otherwise = False

vector x y z = Tuple x y z 0.0

isVector :: Tuple -> Bool
isVector (Tuple _ _ _ w)
  | w == 0    = True
  | otherwise = False
