module Patterns
  ( Pattern (..)
  , stripePattern
  , stripeAt
  , patternAt
  )where

import Tuples
import Matrices

data PatternShape = Stripes
                  deriving(Eq, Show, Ord)

data Pattern = Pattern { a :: Color
                       , b :: Color
                       , patternTransform :: VMatrix
                       , patternShape :: PatternShape}
               deriving(Eq, Show, Ord)

stripePattern :: Color -> Color -> Pattern
stripePattern aC bC = Pattern aC bC identityV Stripes

stripeAt :: Pattern -> Tuple -> Color
stripeAt p point = if (floor (x point)) `mod` 2 == 0
                   then a p
                   else b p

patternAt :: Pattern -> Tuple -> Color
patternAt p@(Pattern { patternShape = ps }) point = case ps of
                                                      Stripes -> stripeAt p point
