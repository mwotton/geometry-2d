{-# LANGUAGE InstanceSigs #-}

module Rectangle where

import Dimension
import Point
import GeometryClasses

newtype Rect2P = Rect2P (Point, Point)

instance Geom Rect2P where
  intersects :: Rect2P -> Rect2P -> Bool
  intersects ra rb = xIntersects && yIntersects
    where (Rect2P (Point (xa0, ya0), Point (xa1, ya1))) = ra
          (Rect2P (Point (xb0, yb0), Point (xb1, yb1))) = rb

          xIntersects = newRange xa0 xa1 `intersects` newRange xb0 xb1
          yIntersects = newRange ya0 ya1 `intersects` newRange yb0 yb1

  parallel = undefined