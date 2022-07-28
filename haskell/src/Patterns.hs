module Patterns
  ( Pattern (..)
  , stripePattern
  , stripeAt
  , gradientPattern
  , patternAt
  )where

import Tuples
import Matrices

data PatternShape = Stripes
                  | Gradient
                  deriving(Eq, Show, Ord)

data Pattern = Pattern { a :: Color
                       , b :: Color
                       , patternTransform :: VMatrix
                       , patternShape :: PatternShape}
               deriving(Eq, Show, Ord)

stripePattern :: Color -> Color -> Pattern
stripePattern aC bC = Pattern aC bC identityV Stripes

stripeAt :: Pattern -> Tuple -> Color
stripeAt p point = if even (floor (x point))
                   then a p
                   else b p

gradientPattern :: Color -> Color -> Pattern
gradientPattern aC bC = Pattern aC bC identityV Gradient

gradientAt :: Pattern -> Tuple -> Color
gradientAt gradient point = let distance = (b gradient) `subC` (a gradient)
                                fraction = (x point) - (fromIntegral (floor (x point)))
                            in (a gradient) `addC` (distance `mulCS` fraction)

patternAt :: Pattern -> Tuple -> Color
patternAt p@Pattern { patternShape = ps } point =
  case ps of
    Stripes  -> stripeAt p point
    Gradient -> gradientAt p point
