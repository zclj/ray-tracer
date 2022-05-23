module Vector
  ( Vector (..)
  , get
  , dot
  , dropAtV
  ) where

data Vector = Vector2D Double Double
            | Vector3D Double Double Double
            | Vector4D Double Double Double Double
  deriving (Show, Ord)

-- Implement Eq based on Epsilon comparison of floats
-- this avoids two floats that are "equal" being evaluated as not equal
-- due too how floats are represented
instance Eq Vector where
  (Vector4D x1 y1 z1 w1) == (Vector4D x2 y2 z2 w2)
    = let epsilon = 0.0001
          ltep    = (\x y -> abs (x - y) < epsilon)
      in ltep x1 x2 && ltep y1 y2 && ltep z1 z2 && ltep w1 w2
  (Vector3D x1 y1 z1) == (Vector3D x2 y2 z2)
    = let epsilon = 0.0001
          ltep    = (\x y -> abs (x - y) < epsilon)
      in ltep x1 x2 && ltep y1 y2 && ltep z1 z2
  (Vector2D x1 y1) == (Vector2D x2 y2)
    = let epsilon = 0.0001
          ltep    = (\x y -> abs (x - y) < epsilon)
      in ltep x1 x2 && ltep y1 y2

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

dot :: Vector -> Vector -> Double
dot (Vector4D x1 y1 z1 w1) (Vector4D x2 y2 z2 w2)
  = x1 * x2 + y1 * y2 + z1 * z2 + w1 * w2

dropAtV :: Vector -> Int -> Vector
dropAtV (Vector4D x1 y1 z1 w1) i
  = case i of
      0 -> Vector3D y1 z1 w1
      1 -> Vector3D x1 z1 w1
      2 -> Vector3D x1 y1 w1
      3 -> Vector3D x1 y1 z1
dropAtV (Vector3D x1 y1 z1) i
  = case i of
      0 -> Vector2D y1 z1
      1 -> Vector2D x1 z1
      2 -> Vector2D x1 y1
