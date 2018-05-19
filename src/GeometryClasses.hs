{-# LANGUAGE MultiParamTypeClasses #-}
module GeometryClasses where

class Absolute a
class Relative a


newtype Scalar = Scalar Double deriving (Show, Eq)
newtype Squared a = Squared a deriving (Show, Eq)


class AbsoluteOps a where
  (.<.) :: Absolute a => a -> a -> Bool
  (.>.) :: Absolute a => a -> a -> Bool
  (.<=.) :: Absolute a => a -> a -> Bool
  (.>=.) :: Absolute a => a -> a -> Bool


infixl 6 =+=
infixl 7 =*=
class RelativeOps r where
  (=+=) :: Relative r => r -> r -> r
  (=-=) :: Relative r => r -> r -> r
  (=*=) :: Relative r => r -> r -> Squared r
  (=/=) :: Relative r => r -> r -> Scalar

  sq :: Relative r => r -> Squared r


infixl 6 .+=
infixl 6 =+.
infixl 6 .-.
class DimensionOps a r where
  (.+=) :: (Absolute a, Relative r) => a -> r -> a
  (=+.) :: (Absolute a, Relative r) => r -> a -> a
  (.-.) :: (Absolute a, Relative r) => a -> a -> r

  _toAbsolute :: (Absolute a, Relative r) => r -> a
  _toRelative :: (Absolute a, Relative r) => a -> r



newtype Area = Area Double deriving (Show, Eq)

class Geom a where
  intersects :: a -> a -> Bool
  parallel :: a -> a -> Bool

  area :: a -> Area