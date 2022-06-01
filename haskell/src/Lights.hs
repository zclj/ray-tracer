module Lights
  ( Light (..)
  , pointLight
  , lighting
  )where

import Tuples
import Materials

data Light = Light { position  :: Tuple
                   , intensity :: Color}
             deriving (Show, Eq)

pointLight :: Tuple -> Color -> Light
pointLight position intensity = Light position intensity

lighting :: Material -> Light -> Tuple -> Tuple -> Tuple -> Color
lighting m l pos eye normal = undefined
