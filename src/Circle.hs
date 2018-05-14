module Circle where

import Dimension
import Point
import Line


newtype Circle = Circle (Point, Radius)

circlesIntersect :: Circle -> Circle -> Bool
circlesIntersect (Circle (p0, r0)) (Circle (p1, r1)) =
  cpdSq < rSq
  where Squared (Length cpdSq) = lenSq $ Line (p0, p1)
        Squared (Radius rSq) = sq $ r0 =+= r1
