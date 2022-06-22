module Shapes where

import Matrices
import Materials
import Tuples

class Shape a where
  shapeTransform :: a -> VMatrix
  shapeMaterial  :: a -> Material
  shapeNormalAt  :: a -> Tuple -> Tuple

-- getTransform :: (Shape a) => a -> VMatrix
-- getTransform s = transform s


