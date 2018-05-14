module Point where

import Dimension (Ax, Ay)

newtype Point = Point (Ax, Ay) deriving (Show)
