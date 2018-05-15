module Vector where

import Dimension
import Point

newtype Vector = Vector (Rx, Ry) deriving (Show, Eq)
newtype ZVector = ZVector Double deriving (Show, Eq)

-- 2d cross product
x :: Vector -> Vector -> ZVector
x u v = ZVector z
  where Vector (u_x, u_y) = u
        Vector (v_x, v_y) = v
        Rz z = (u_x =**= v_y) =-= (v_x =**= u_y)


makeVector :: Double -> Double -> Vector
makeVector x y = Vector (Rx x, Ry y)

diffPoints :: Point -> Point -> Vector
diffPoints (Point (x0, y0)) (Point (x1, y1)) = Vector (x1 .-. x0, y1 .-. y0)

isPos :: ZVector -> Bool
isPos (ZVector z) = z > 0
