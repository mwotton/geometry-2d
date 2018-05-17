module GeometryClasses where

class Geom a where
  intersects :: a -> a -> Bool
  parallel :: a -> a -> Bool

