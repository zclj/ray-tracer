module Patterns
  ( Pattern (..)
  , stripePattern
  , stripeAt
  )where

import Tuples

data Pattern = Pattern { a :: Color
                       , b :: Color }
               deriving(Eq, Show, Ord)

stripePattern :: Color -> Color -> Pattern
stripePattern aC bC = Pattern aC bC

stripeAt :: Pattern -> Tuple -> Color
stripeAt p point = if (floor (x point)) `mod` 2 == 0
                   then a p
                   else b p
