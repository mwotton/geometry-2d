module Line where

import Dimension
import Point

newtype Length = Length Double deriving (Show, Eq)
newtype Line = Line (Point, Point) deriving (Show, Eq)

-- squared length of line
lenSq :: Line -> Squared Length
lenSq (Line ((Point (x0, y0)), Point (x1, y1))) = Squared (Length magnitude)
  where delX = x0 .-. x1
        delY = y0 .-. y1
        Squared (Scalar magnitude) = (sq delX) `xSqAdd` (sq delY)
