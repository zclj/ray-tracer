module Shapes where

import Matrices
import Materials

class Shape a where
  transform :: a -> VMatrix
  material  :: a -> Material

-- transform :: (Shape a) => a -> VMatrix
-- transform s = transform s


