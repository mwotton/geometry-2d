module Point where

import Dimension

newtype Point = Point (Ax, Ay) deriving (Show, Eq)

pt :: Double -> Double -> Point
pt x y = Point (Ax x, Ay y)
