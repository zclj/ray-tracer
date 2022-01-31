module Tuples where

data Tuple = Tuple { x :: Double
                   , y :: Double
                   , z :: Double
                   , w :: Double}
             deriving (Show)

-- Implement Eq based on Epsilon comparison of floats
-- this avoids two floats that are "equal" being evaluated as not equal
-- due too how floats are represented
instance Eq Tuple where
  (Tuple x1 y1 z1 w1) == (Tuple x2 y2 z2 w2)
    = let epsilon = 0.0001
          ltep    = (\x y -> abs (x - y) < epsilon)
      in (ltep x1 x2) && (ltep y1 y2) && (ltep z1 z2) && (ltep w1 w2)  

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

neg :: Tuple -> Tuple
neg (Tuple x y z w) = (Tuple (-x) (-y) (-z) (-w))

mul :: Tuple -> Double -> Tuple
mul (Tuple x y z w) s = Tuple (x * s) (y * s) (z * s) (w * s)

div :: Tuple -> Double -> Tuple
div (Tuple x y z w) s = Tuple (x / s) (y / s) (z / s) (w / s)

mag :: Tuple -> Double
mag (Tuple x y z w) = sqrt $ x^2 + y^2 + z^2 + w^2

norm :: Tuple -> Tuple
norm t@(Tuple x y z w) = let m = (mag t)
                         in Tuple (x / m) (y / m) (z / m) (w / m)

dot :: Tuple -> Tuple -> Double
dot (Tuple x1 y1 z1 w1) (Tuple x2 y2 z2 w2)
  = x1 * x2 + y1 * y2 + z1 * z2 + w1 * w2 

cross :: Tuple -> Tuple -> Tuple
cross (Tuple x1 y1 z1 w1) (Tuple x2 y2 z2 w2)
  = vector (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2) 

{- Colors -}
epsilon = 0.0001

(~=) :: Double -> Double -> Bool
(~=) x y = abs (x - y) < epsilon

newtype Red = Red Double
  deriving (Show)

instance Eq Red where
  (Red r1) == (Red r2) =  r1 ~= r2

newtype Green = Green Double
  deriving (Show)

instance Eq Green where
  (Green g1) == (Green g2) =  g1 ~= g2

newtype Blue = Blue Double
  deriving (Show)

instance Eq Blue where
  (Blue b1) == (Blue b2) =  b1 ~= b2

data Color = Color { red :: Red
                   , green :: Green
                   , blue :: Blue }
             deriving (Show, Eq)

addC :: Color -> Color -> Color
addC (Color (Red r1) (Green g1) (Blue b1)) (Color (Red r2) (Green g2) (Blue b2)) =
  let (Tuple r g b _) = Tuple r1 g1 b1 0 `add` Tuple r2 g2 b2 0
  in Color (Red r) (Green g) (Blue b)
  
subC :: Color -> Color -> Color
subC (Color (Red r1) (Green g1) (Blue b1)) (Color (Red r2) (Green g2) (Blue b2)) =
  let (Tuple r g b _) = Tuple r1 g1 b1 0 `sub` Tuple r2 g2 b2 0
  in Color (Red r) (Green g) (Blue b)

mulCS :: Color -> Double -> Color
mulCS (Color (Red r1) (Green g1) (Blue b1)) x =
  let (Tuple r g b _) = Tuple r1 g1 b1 0 `mul` x
  in Color (Red r) (Green g) (Blue b)
  
mulC :: Color -> Color -> Color
mulC (Color (Red r1) (Green g1) (Blue b1)) (Color (Red r2) (Green g2) (Blue b2)) =
  let r = (Red (r1 * r2))
      g = (Green (g1 * g2))
      b = (Blue (b1 * b2))
  in Color r g b
