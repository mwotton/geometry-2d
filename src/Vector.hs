module Vector where

import Point

newtype Vector = Vector Point deriving (Show)
newtype ZVector = ZVector Double deriving (Show)

crossProduct :: Vector -> Vector -> ZVector
-- crossProduct u v = (u_x * v_y - u_y * v_x)
crossProduct = undefined
