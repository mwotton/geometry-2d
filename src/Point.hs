module Point where

import GeometryClasses
import Dimension
import Vector

newtype Point = Point (Ax, Ay) deriving (Show, Eq)

pt :: Double -> Double -> Point
pt x y = Point (Ax x, Ay y)

diffPoints :: Point -> Point -> Vector
diffPoints (Point (x0, y0)) (Point (x1, y1)) = Vector (x1 .-. x0, y1 .-. y0)
(.->.) = diffPoints
(.<-.) = flip diffPoints

pointPlusVector :: Point -> Vector -> Point
pointPlusVector (Point (px, py)) (Vector (vx, vy)) = Point (px .+= vx, py .+= vy)
(.+->) = pointPlusVector

vector2point :: Vector -> Point
vector2point (Vector (rx, ry)) = Point (_toAbsolute rx, _toAbsolute ry)
