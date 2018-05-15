module Triangle where

import Point
import Line


newtype Triangle = Triangle (Line, Line, Line) deriving (Show, Eq)

triangleFromPoints :: Point -> Point -> Point -> Triangle
triangleFromPoints p0 p1 p2 = Triangle (Line (p0, p1), Line (p1, p2), Line (p2, p0))

contains :: Triangle -> Point -> Bool
contains t p = undefined
