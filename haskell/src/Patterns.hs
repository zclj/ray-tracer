module Patterns
  ( Pattern (..)
  , stripePattern
  , stripeAt
  , gradientPattern
  , patternAt
  , ringPattern
  , checkersPattern
  , pointPattern
  )where

import Tuples
import Matrices

data PatternShape = Stripes
                  | Gradient
                  | Ring
                  | Checkers
                  | Point
                  deriving(Eq, Show, Ord)

data Pattern = Pattern { a :: Color
                       , b :: Color
                       , patternTransform :: VMatrix
                       , patternShape :: PatternShape}
               deriving(Eq, Show, Ord)

pointPattern :: Pattern
pointPattern = Pattern (Color (Red 0) (Green 0) (Blue 0)) (Color (Red 0) (Green 0) (Blue 0)) identityV Point

pointAt :: Pattern -> Tuple -> Color
pointAt p (Tuple x y z _) = Color (Red x) (Green y) (Blue z)

stripePattern :: Color -> Color -> Pattern
stripePattern aC bC = Pattern aC bC identityV Stripes

stripeAt :: Pattern -> Tuple -> Color
stripeAt p point = if even (floor (x point))
                   then a p
                   else b p

gradientPattern :: Color -> Color -> Pattern
gradientPattern aC bC = Pattern aC bC identityV Gradient

gradientAt :: Pattern -> Tuple -> Color
gradientAt gradient point = let distance = b gradient `subC` a gradient
                                fraction = x point - fromIntegral (floor (x point))
                            in a gradient `addC` distance `mulCS` fraction

ringPattern :: Color -> Color -> Pattern
ringPattern a b = Pattern a b identityV Ring

ringAt :: Pattern -> Tuple -> Color
ringAt ring point = if even (floor (sqrt (x point ** 2 + z point ** 2)))
                    then a ring
                    else b ring

checkersPattern :: Color -> Color -> Pattern
checkersPattern a b = Pattern a b identityV Checkers

checkersAt :: Pattern -> Tuple -> Color
checkersAt checkers (Tuple x y z _) = if even (floor x + floor y + floor z)
                                      then a checkers
                                      else b checkers

patternAt :: Pattern -> Tuple -> Color
patternAt p@Pattern { patternShape = ps } point =
  case ps of
    Stripes  -> stripeAt p point
    Gradient -> gradientAt p point
    Ring     -> ringAt p point
    Checkers -> checkersAt p point
    Point    -> pointAt p point
