{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module Dimension where


class Absolute a
class Relative a

newtype Scalar a = Scalar a deriving (Show, Eq)
newtype Squared a = Squared a deriving (Show, Eq)

infixl 6 =+=
infixl 7 =*=
class RelativeOps r where
  (=+=) :: Relative r => r -> r -> r
  (=-=) :: Relative r => r -> r -> r
  (=*=) :: Relative r => r -> r -> Squared r

  sq :: Relative r => r -> Squared r


infixl 6 .+=
infixl 6 =+.
infixl 6 .-.
class DimensionOps a r where
  (.+=) :: (Absolute a, Relative r) => a -> r -> a
  (=+.) :: (Absolute a, Relative r) => r -> a -> a
  (.-.) :: (Absolute a, Relative r) => a -> a -> r

  _toAbsolute :: (Absolute a, Relative r) => r -> a



-- absolute x
newtype Ax = Ax Double deriving (Show, Eq)
instance Absolute Ax

-- relative x
newtype Rx = Rx Double deriving (Show, Eq)
instance Relative Rx
instance RelativeOps Rx where
  (=+=) :: Rx -> Rx -> Rx
  (=+=) (Rx r0) (Rx r1) = Rx (r0 + r1)

  (=-=) :: Rx -> Rx -> Rx
  (=-=) (Rx r0) (Rx r1) = Rx (r0 - r1)

  (=*=) :: Rx -> Rx -> Squared Rx
  (=*=) (Rx r0) (Rx r1) = Squared (Rx (r0 * r1))

  sq :: Rx -> Squared Rx
  sq r = r =*= r

instance DimensionOps Ax Rx where
  (.+=) :: Ax -> Rx -> Ax
  (.+=) (Ax a) (Rx r) = Ax (a + r)

  (=+.) :: Rx -> Ax -> Ax
  (=+.) = flip (.+=)

  (.-.) :: Ax -> Ax -> Rx
  (.-.) (Ax a0) (Ax a1) = Rx (a0 - a1)

  _toAbsolute :: Rx -> Ax
  _toAbsolute (Rx v) = Ax v


-- absolute y
newtype Ay = Ay Double deriving (Show, Eq)
instance Absolute Ay

-- relative y
newtype Ry = Ry Double deriving (Show, Eq)
instance Relative Ry
instance RelativeOps Ry where
  (=+=) :: Ry -> Ry -> Ry
  (=+=) (Ry r0) (Ry r1) = Ry (r0 + r1)

  (=-=) :: Ry -> Ry -> Ry
  (=-=) (Ry r0) (Ry r1) = Ry (r0 - r1)

  (=*=) :: Ry -> Ry -> Squared Ry
  (=*=) (Ry r0) (Ry r1) = Squared (Ry (r0 * r1))

  sq :: Ry -> Squared Ry
  sq r = r =*= r

instance DimensionOps Ay Ry where
  (.+=) :: Ay -> Ry -> Ay
  (.+=) (Ay a) (Ry r) = Ay (a + r)

  (=+.) :: Ry -> Ay -> Ay
  (=+.) = flip (.+=)

  (.-.) :: Ay -> Ay -> Ry
  (.-.) (Ay a0) (Ay a1) = Ry (a0 - a1)

  _toAbsolute :: Ry -> Ay
  _toAbsolute (Ry v) = Ay v


-- absolute z
newtype Az = Az Double deriving (Show, Eq)
instance Absolute Az

-- relative z
newtype Rz = Rz Double deriving (Show, Eq)
instance Relative Rz

instance RelativeOps Rz where
  (=+=) :: Rz -> Rz -> Rz
  (=+=) (Rz r0) (Rz r1) = Rz (r0 + r1)

  (=-=) :: Rz -> Rz -> Rz
  (=-=) (Rz r0) (Rz r1) = Rz (r0 - r1)

  (=*=) :: Rz -> Rz -> Squared Rz
  (=*=) (Rz r0) (Rz r1) = Squared (Rz (r0 * r1))

  sq :: Rz -> Squared Rz
  sq r = r =*= r

-- instance DimensionOps Az Rz where
--   (.+=) :: Az -> Rz -> Az
--   (.+=) (Az a) (Rz r) = Az (a + r)

--   (=+.) :: Rz -> Az -> Az
--   (=+.) = flip (.+=)

--   (.-.) :: Az -> Az -> Rz
--   (.-.) (Az a0) (Az a1) = Rz (a0 - a1)

--   _toAbsolute :: Rz -> Az
--   _toAbsolute (Rz v) = Az v


infixl 7 =**=
(=**=) :: Rx -> Ry -> Rz
(=**=) (Rx x) (Ry y) = Rz (x * y)


xSqAdd :: Squared Rx -> Squared Ry -> Squared (Scalar Double)
xSqAdd (Squared (Rx x2)) (Squared (Ry y2)) = Squared (Scalar (x2 + y2))




newtype Radius = Radius Double deriving (Show)
instance Relative Radius

instance RelativeOps Radius where
  (=+=) :: Radius -> Radius -> Radius
  (=+=) (Radius r0) (Radius r1) = Radius (r0 + r1)

  (=-=) :: Radius -> Radius -> Radius
  (=-=) (Radius r0) (Radius r1) = Radius (r0 - r1)

  (=*=) :: Radius -> Radius -> Squared Radius
  (=*=) (Radius r0) (Radius r1) = Squared (Radius (r0 * r1))

  sq :: Radius -> Squared Radius
  sq r = r =*= r