module Tuples where

data Tuple = Tuple { x :: Double
                   , y :: Double
                   , z :: Double
                   , w :: Double}
             deriving (Show, Eq)

point :: Double -> Double -> Double -> Tuple
point x y z = Tuple x y z 1.0

isPoint :: Tuple -> Bool
isPoint (Tuple _ _ _ w) = w == 1

vector :: Double -> Double -> Double -> Tuple
vector x y z = Tuple x y z 0.0

isVector :: Tuple -> Bool
isVector (Tuple _ _ _ w) = w == 0

add :: Tuple -> Tuple -> Tuple
add (Tuple x1 y1 z1 w1) (Tuple x2 y2 z2 w2)
  = Tuple (x1 + x2) (y1 + y2) (z1 + z2) (w1 + w2)

sub :: Tuple -> Tuple -> Tuple
sub (Tuple x1 y1 z1 w1) (Tuple x2 y2 z2 w2)
  = Tuple (x1 - x2) (y1 - y2) (z1 - z2) (w1 - w2)
