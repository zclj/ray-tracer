module Vector
  ( Vector (..)
  , get
  ) where

-- data Vector3D = Vector3D Double Double Double
--   deriving (Show, Eq)

-- data Vector4D = Vector4D Double Double Double Double
--   deriving (Show, Eq)

data Vector = Vector3D Double Double Double
            | Vector4D Double Double Double Double
  deriving (Show, Eq)

fromList3D :: [Double] -> Vector
fromList3D (x:y:z:_) = (Vector3D x y z)

fromList4D :: [Double] -> Vector
fromList4D (x:y:z:w:_) = (Vector4D x y z w)

toList3D :: Vector -> [Double]
toList3D (Vector3D x y z) = [x, y, z]

toList4D :: Vector -> [Double]
toList4D (Vector4D x y z w) = [x, y, z, w]

get :: Vector -> Int -> Double
get (Vector4D x y z w) i
  = case i of
      0 -> x
      1 -> y
      2 -> z
      3 -> w
get (Vector3D x y z) i
  = case i of
      0 -> x
      1 -> y
      2 -> z
