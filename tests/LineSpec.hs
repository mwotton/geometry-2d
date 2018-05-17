module LineSpec where

import Dimension
import GeometryClasses
import Vector
import Point
import Line

import Test.Hspec (hspec, it, describe, shouldBe, Spec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "lenSq (squared length)" $ do
    let p0 = Point (Ax 1, Ay 2)
    let p1 = Point (Ax 3, Ay 5)
    let ln = Line (p0, p1)
    lenSq ln `shouldBe` Squared (Length 13)

  describe "intersects" $ do
    it "parallel intersect" $ do
      Line ((pt 1 1), (pt 2 2)) `intersects` Line ((pt 0 0), (pt 3 3)) `shouldBe` True
    it "shared endpoint" $ do
      Line ((pt 0 0), (pt 1 1)) `intersects` Line ((pt 1 1), (pt 2 0)) `shouldBe` True
    it "crosses" $ do
      Line ((pt 0 0), (pt 1 1)) `intersects` Line ((pt 0 1), (pt 1 0)) `shouldBe` True
    it "no cross" $ do
      Line ((pt 0 0), (pt 3 0)) `intersects` Line ((pt 0 2), (pt 1 1)) `shouldBe` False
    it "parallel no intersect" $ do
      Line ((pt 0 0), (pt 1 1)) `intersects` Line ((pt 0 1), (pt 1 2)) `shouldBe` False
    it "vertical horizontal intersect" $ do
      Line ((pt 1 0), (pt 1 2)) `intersects` Line ((pt 0 1), (pt 2 1)) `shouldBe` True
    it "vertical vertical intersect" $ do
      Line ((pt 0 0), (pt 0 2)) `intersects` Line ((pt 0 1), (pt 0 3)) `shouldBe` True
    it "vertical horizontal no intersect" $ do
      Line ((pt 1 0), (pt 1 1)) `intersects` Line ((pt 0 2), (pt 2 2)) `shouldBe` False
    it "vertical vertical no intersect" $ do
      Line ((pt 0 0), (pt 0 1)) `intersects` Line ((pt 1 0), (pt 1 1)) `shouldBe` False

  it "l2v" $ do
    let p0 = pt 1 2
    let p1 = pt 3 5
    l2v (Line (p0, p1)) `shouldBe` (p0 .->. p1)

  describe "parallel" $ do
    it "offset" $ do
      (Line ((pt 0 0), (pt 1 1))) `parallel` (Line ((pt 0 1), (pt 1 2))) `shouldBe` True
    it "colinear" $ do
      (Line ((pt 0 0), (pt 1 1))) `parallel` (Line ((pt 3 3), (pt 2 2))) `shouldBe` True
    it "not parallel" $ do
      (Line ((pt 0 0), (pt 1 1))) `parallel` (Line ((pt 0 0), (pt 1 2))) `shouldBe` False
