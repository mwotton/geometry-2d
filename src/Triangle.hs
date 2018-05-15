module Triangle where

import Point
import Line


newtype Triangle = Triangle {
    pts :: (Point, Point, Point)
    area :: 
  }

deriving (Show, Eq)




triangleFromPoints :: Point -> Point -> Point -> Triangle
triangleFromPoints p0 p1 p2 = Triangle (Line (p0, p1), Line (p1, p2), Line (p2, p0))

contains :: Triangle -> Point -> Bool
contains t p = undefined



s = 1/(2*Area)*(p0y*p2x - p0x*p2y + (p2y - p0y)*px + (p0x - p2x)*py);
t = 1/(2*Area)*(p0x*p1y - p0y*p1x + (p0y - p1y)*px + (p1x - p0x)*py);
Area = 0.5 *(-p1y*p2x + p0y*(-p1x + p2x) + p0x*(p1y - p2y) + p1x*p2y);
Just evaluate s, t and 1-s-t.
The point p is inside the triangle if and only if they are all positive.