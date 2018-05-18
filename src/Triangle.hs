{-# LANGUAGE InstanceSigs #-}

module Triangle where

import GeometryClasses
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


edges :: Triangle -> [Line]
edges t = [p0 ..- p1, p1 ..- p2, p2 ..- p0]
          where (p0, p1, p2) = points t


instance Geom Triangle where
  intersects :: Triangle -> Triangle -> Bool
  intersects t0 t1 = crosses || contained
    where crosses = any id [e0 `intersects` e1 | e0 <- edges t0, e1 <- edges t1]
          contained = containsOther t0 t1 || containsOther t1 t0
          containsOther t0 t1 = t0 `contains` (fst3 (points t1))

          fst3 (x,_,_) = x

  parallel = undefined
