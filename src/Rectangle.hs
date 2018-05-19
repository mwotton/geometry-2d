{-# LANGUAGE InstanceSigs #-}

module Rectangle where

import GeometryClasses
import Range
import Dimension
import Vector
import Point


newtype RectVec = RectVec Vector
instance Geom RectVec where
  area r = Area (abs a)
    where RectVec (Vector (rx, ry)) = r
          Rz a = rx =**= ry

  intersects = undefined
  parallel = undefined


newtype Rect2P = Rect2P (Point, Point)
instance Geom Rect2P where
  intersects :: Rect2P -> Rect2P -> Bool
  intersects ra rb = xIntersects && yIntersects
    where (Rect2P (Point (xa0, ya0), Point (xa1, ya1))) = ra
          (Rect2P (Point (xb0, yb0), Point (xb1, yb1))) = rb

          xIntersects = newRange xa0 xa1 `intersects` newRange xb0 xb1
          yIntersects = newRange ya0 ya1 `intersects` newRange yb0 yb1

  parallel = undefined
  area = undefined


-- -- minimum bounding box (horizontal & vertical sides)
-- mbb :: [Point] -> Rect2P
-- mbb (p:ps) = Rect2P (foldr minMax (p, p) ps)
--   where minMax p (minP, maxP) = (minimums p minP, maximums p maxP)
--         minimums = selectDim (.<.)
--         maximums = selectDim (.>.)

--         -- selectDim :: AbsoluteOps a => (a -> a -> Bool) -> Point -> Point -> Point
--         selectDim cmp (Point (x0, y0)) (Point (x1, y1)) =
--           Point (selectPos x0 x1, selectPos y0 y1)
--           where selectPos :: Absolute b => b -> b -> b
--                 selectPos p0 p1 = if (p0 `cmp` p1) then p0 else p1
