module Shapes where

import Matrices
import Materials

class Shape a where
  shapeTransform :: a -> VMatrix
  shapeMaterial  :: a -> Material

-- getTransform :: (Shape a) => a -> VMatrix
-- getTransform s = transform s


