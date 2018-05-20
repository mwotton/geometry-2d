{-# LANGUAGE InstanceSigs #-}

module Rectangle where

import GeometryClasses
import Range
import Dimension
import Vector
import Point


newtype RectVec = RectVec Vector deriving (Show, Eq)
instance Geom RectVec where
  area r = Area (abs a)
    where RectVec (Vector (rx, ry)) = r
          Rz a = rx =**= ry

  intersects = undefined
  parallel = undefined


newtype Rect2P = Rect2P (Point, Point) deriving (Show, Eq)
instance Geom Rect2P where
  intersects :: Rect2P -> Rect2P -> Bool
  intersects ra rb = xIntersects && yIntersects
    where (Rect2P (Point (xa0, ya0), Point (xa1, ya1))) = ra
          (Rect2P (Point (xb0, yb0), Point (xb1, yb1))) = rb

          xIntersects = newRange xa0 xa1 `intersects` newRange xb0 xb1
          yIntersects = newRange ya0 ya1 `intersects` newRange yb0 yb1

  parallel = undefined
  area = undefined


-- minimum bounding box (horizontal & vertical sides)
mbb :: [Point] -> Rect2P
mbb (p:ps) = Rect2P (foldr minMax (p, p) ps)
  where minMax :: Point -> (Point, Point) -> (Point, Point)
        minMax p (minP, maxP) = (minimums p minP, maximums p maxP)

        minimums (Point (x0, y0)) (Point (x1, y1)) = Point (min x0 x1, min y0 y1)
        maximums (Point (x0, y0)) (Point (x1, y1)) = Point (max x0 x1, max y0 y1)
