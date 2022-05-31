module Lights
  ( Light (..)
  , pointLight
  )where

import Tuples

data Light = Light { position  :: Tuple
                   , intensity :: Color}
             deriving (Show, Eq)

pointLight :: Tuple -> Color -> Light
pointLight position intensity = Light position intensity
