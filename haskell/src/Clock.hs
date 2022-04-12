module Clock where

import Transformations
import Tuples
import Matrices

{- Make the hour points of a clock with matrix transformations -}

origin = point 400 400 0

twelve = mulT (transform [translation 0 (-20) 0]) origin
