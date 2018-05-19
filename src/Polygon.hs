module Polygon where

import Point
import Triangle


newtype Polygon = Polygon ([Point], [[Point]]) deriving (Show, Eq)

triangulate :: Polygon -> [Triangle]
triangulate (Polygon (e, is)) = undefined
