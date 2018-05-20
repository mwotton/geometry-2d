{-# LANGUAGE InstanceSigs #-}
module Range where

import GeometryClasses


newtype Range a = Range (a, a) deriving (Show, Eq)

newRange :: (Absolute a, Ord a) => a -> a -> Range a
-- newRange r0 r1 = if r0 .<. r1
newRange r0 r1 = if r0 < r1
                 then Range (r0, r1)
                 else Range (r1, r0)

inRange :: (Absolute a, Ord a) => Range a -> a -> Bool
inRange (Range (l, u)) v = v >= l && v <= u


instance (Absolute a, Ord a) => Geom (Range a) where
  intersects :: Range a -> Range a -> Bool
  intersects r0@(Range (l0, u0)) r1@(Range (l1, u1)) =
    inRange r0 l1 || inRange r0 u1 || inRange r1 l0 || inRange r1 u0

  parallel = undefined
  area = undefined
