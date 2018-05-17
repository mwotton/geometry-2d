{-# LANGUAGE InstanceSigs #-}
module Vector where

import Dimension
import GeometryClasses


newtype Vector = Vector (Rx, Ry) deriving (Show, Eq)

-- 2d cross product
x :: Vector -> Vector -> Rz
x u v = Rz z
  where Vector (u_x, u_y) = u
        Vector (v_x, v_y) = v
        Rz z = (u_x =**= v_y) =-= (v_x =**= u_y)


makeVector :: Double -> Double -> Vector
makeVector x y = Vector (Rx x, Ry y)

isPos :: Rz -> Bool
isPos (Rz z) = z > 0

isNeg :: Rz -> Bool
isNeg (Rz z) = z < 0

instance Geom Vector where
  intersects = undefined

  parallel :: Vector -> Vector -> Bool
  parallel v0 v1 = v0 `x` v1 == Rz 0
