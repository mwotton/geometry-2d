module Triangle where

import Point
import Line
import Vector


data Triangle = Triangle {
    points :: (Point, Point, Point)
  , vectors :: (Vector, Vector, Vector)
  } deriving (Show, Eq)

makeTriangle :: Point -> Point -> Point -> Triangle
makeTriangle p0 p1 p2 =
  if isPos (v01 `x` v12)
  then Triangle {
      points = (p0, p1, p2)
    , vectors = (v01 ,v12 ,v20)
    }
  else Triangle {
      points = (p0, p2, p1)
    , vectors = (v02, v21, v10)
    }
  where v01 = p0 .->. p1
        v12 = p1 .->. p2
        v20 = p2 .->. p0

        v02 = p0 .->. p2
        v21 = p2 .->. p1
        v10 = p1 .->. p0


-- Reference: https://math.stackexchange.com/a/51328
contains :: Triangle -> Point -> Bool
contains t p = if as
               then bs && cs
               else not bs && not cs
  where (a, b, c) = points t
        (ab, bc, ca) = vectors t
        ap = a .->. p
        bp = b .->. p
        cp = c .->. p

        as = isNeg (ab `x` ap)
        bs = isNeg (bc `x` bp)
        cs = isNeg (ca `x` cp)


-- intersects :: Triangle -> Triangle -> Bool
-- intersects 
