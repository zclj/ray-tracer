module TuplesWithClass where

class RTTuple a where
  x1 :: a -> Double

data Point = Point { x :: Double
                   , y :: Double }

instance RTTuple Point where
  x1 (Point x _) = x

data Vector = Vector Double Double
            deriving(Show, Eq)

instance RTTuple Vector where
  x1 (Vector x _) = x

mul2 :: RTTuple a => a -> Double -> a
mul2 t s = (RTTuple a (x t) (x t))
