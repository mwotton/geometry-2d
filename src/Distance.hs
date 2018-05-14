{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module Distance where


class Absolute a
class Relative a

newtype Scalar a = Scalar a deriving (Show, Eq)
newtype Squared a = Squared a deriving (Show, Eq)

class RelativeOps r where
  (=+=) :: Relative r => r -> r -> r
  (=*=) :: Relative r => r -> r -> Squared r

  sq :: Relative r => r -> Squared r

class AbsoluteOps a where

class DimensionOps a r where
  (.+=) :: (Absolute a, Relative r) => a -> r -> a
  (=+.) :: (Absolute a, Relative r) => r -> a -> a
  (.-.) :: (Absolute a, Relative r) => a -> a -> r



-- absolute x
newtype Ax = Ax Double deriving (Show, Eq)
instance Absolute Ax

-- relative x
newtype Rx = Rx Double deriving (Show, Eq)
instance Relative Rx
instance RelativeOps Rx where
  (=+=) :: Rx -> Rx -> Rx
  (=+=) (Rx r0) (Rx r1) = Rx (r0 + r1)

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



-- absolute x
newtype Ay = Ay Double deriving (Show, Eq)
instance Absolute Ay

-- relative y
newtype Ry = Ry Double deriving (Show, Eq)
instance Relative Ry
instance RelativeOps Ry where
  (=+=) :: Ry -> Ry -> Ry
  (=+=) (Ry r0) (Ry r1) = Ry (r0 + r1)

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


xSqAdd :: Squared Rx -> Squared Ry -> Squared (Scalar Double)
xSqAdd (Squared (Rx x2)) (Squared (Ry y2)) = Squared (Scalar (x2 + y2))



newtype Length = Length Double deriving (Show, Eq)

newtype Point = Point (Ax, Ay) deriving (Show)

newtype Line = Line (Point, Point) deriving (Show)

-- squared length of line
lenSq :: Line -> Squared Length
lenSq (Line ((Point (x0, y0)), Point (x1, y1))) = Squared (Length magnitude)
  where delX = x0 .-. x1
        delY = y0 .-. y1
        Squared (Scalar magnitude) = (sq delX) `xSqAdd` (sq delY)



newtype Radius = Radius Double deriving (Show)
instance Relative Radius

instance RelativeOps Radius where
  (=+=) :: Radius -> Radius -> Radius
  (=+=) (Radius r0) (Radius r1) = Radius (r0 + r1)

  (=*=) :: Radius -> Radius -> Squared Radius
  (=*=) (Radius r0) (Radius r1) = Squared (Radius (r0 * r1))

  sq :: Radius -> Squared Radius
  sq r = r =*= r

newtype Circle = Circle (Point, Radius)

circlesIntersect :: Circle -> Circle -> Bool
circlesIntersect (Circle (p0, r0)) (Circle (p1, r1)) =
  cpdSq < rSq
  where Squared (Length cpdSq) = lenSq $ Line (p0, p1)
        Squared (Radius rSq) = sq $ r0 =+= r1




