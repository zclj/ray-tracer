module Vector
  ( Vector3D
  , Vector4D
  ) where

data Vector3D a = Vector3D !a !a !a

data Vector4D a = Vector4D !a !a !a !a

fromList3D :: [a] -> Vector3D a
fromList3D (x:y:z:_) = (Vector3D x y z)

fromList4D :: [a] -> Vector4D a
fromList4D (x:y:z:w:_) = (Vector4D x y z w)

toList3D :: Vector3D a -> [a]
toList3D (Vector3D x y z) = [x, y, z]

toList4D :: Vector4D a -> [a]
toList4D (Vector4D x y z w) = [x, y, z, w]
