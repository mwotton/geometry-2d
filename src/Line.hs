{-# LANGUAGE InstanceSigs #-}

module Line where

import Dimension
import GeometryClasses
import Point
import Vector


newtype Line = Line (Point, Point) deriving (Show, Eq)

newtype Length = Length Double deriving (Show, Eq)

instance Geom Line where
  -- Reference: https://stackoverflow.com/a/565282/6933270
  intersects :: Line -> Line -> Bool
  intersects lp@(Line (p, pe)) lq@(Line (q, qe)) =
    
    where Scalar t = ((q .<-. p) `x` s) =/= (r `x` s)
          Scalar u = ((p .<-. q) `x` r) =/= (s `x` r)
          r = p .->. pe
          s = q .->. qe

          isIntersectPoint = t >= 0 && t <= 1 && u >= 0 && u <= 1
          isParallel = isNaN t


  -- line vectors could be stashed
  parallel :: Line -> Line -> Bool
  parallel l0 l1 = (l2v l0) `x` (l2v l1) == Rz 0

-- squared length of line
lenSq :: Line -> Squared Length
lenSq (Line ((Point (x0, y0)), Point (x1, y1))) = Squared (Length magnitude)
  where delX = x0 .-. x1
        delY = y0 .-. y1
        Squared (Scalar magnitude) = (sq delX) `xSqAdd` (sq delY)

l2v :: Line -> Vector
l2v (Line (p0, p1)) = p0 .->. p1

pointOnLine :: Point -> Line -> Bool


p + t*pe = q
t*pe = q - p
t = (q - p) / pe
