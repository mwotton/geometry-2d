{-# LANGUAGE InstanceSigs #-}

module Line where

import Dimension
import GeometryClasses
import Vector
import Point
import Rectangle

newtype Line = Line (Point, Point) deriving (Show, Eq)

newtype Length = Length Double deriving (Show, Eq)

instance Geom Line where

  -- Reference: https://stackoverflow.com/a/565282/6933270
  -- Optimization Improvement: isOverlap checks both x and y. x could only be checked unless vertial and then y could be checked
  intersects :: Line -> Line -> Bool
  intersects lp@(Line (p, pe)) lq@(Line (q, qe)) = 
    isIntersectPoint || isParallelIntersect
    where Scalar t = ((q .<-. p) `x` s) =/= (r `x` s)
          Scalar u = ((p .<-. q) `x` r) =/= (s `x` r)
          r = p .->. pe
          s = q .->. qe

          isIntersectPoint = t >= 0 && t <= 1 && u >= 0 && u <= 1
          isParallelIntersect = isParallel && isColinear && isOverlap

          isParallel = isNaN t
          isColinear = r `parallel` (p .->. q)
          isOverlap = Rect2P (p, pe) `intersects` Rect2P (q, qe)


  -- line vectors could be stashed
  parallel :: Line -> Line -> Bool
  parallel l0 l1 = (l2v l0) `x` (l2v l1) == Rz 0

  area = undefined

-- squared length of line
lenSq :: Line -> Squared Length
lenSq (Line ((Point (x0, y0)), Point (x1, y1))) = Squared (Length magnitude)
  where delX = x0 .-. x1
        delY = y0 .-. y1
        Squared (Scalar magnitude) = (sq delX) `xSqAdd` (sq delY)

l2v :: Line -> Vector
l2v (Line (p0, p1)) = p0 .->. p1

(..-) :: Point -> Point -> Line
(..-) p0 p1 = Line (p0, p1)