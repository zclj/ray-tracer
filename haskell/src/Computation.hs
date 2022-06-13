module Computation
  ( Computation (..)
  ) where

import Tuples
import Spheres

data Computation = Computation { t       :: Double
                               , object  :: Sphere
                               , point   :: Tuple
                               , eyev    :: Tuple
                               , normalv :: Tuple
                               , inside  :: Bool}
                   deriving(Show)
