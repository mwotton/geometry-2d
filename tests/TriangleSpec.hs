module TriangleSpec where

import Dimension
import Point
import Line
import Vector
import Triangle

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "makeTriangle" $ do
    it "ccw to ccw" $ do
      let pts = (pt 0 0, pt 1 0, pt 1 1)
      let (p0, p1, p2) = pts
      let refTri = Triangle {
          points = pts
        , vectors = (p0 .->. p1, p1 .->. p2, p2 .->. p0)
        }
      makeTriangle p0 p1 p2 `shouldBe` refTri

    it "cw to ccw" $ do
      let pts = (pt 0 0, pt 1 0, pt 1 1)
      let (p0, p1, p2) = pts
      let refTri = Triangle {
          points = pts
        , vectors = (p0 .->. p1, p1 .->. p2, p2 .->. p0)
        }
      makeTriangle p0 p2 p1 `shouldBe` refTri

  describe "triangle contains point" $ do
    it "inside" $ do
      let t = makeTriangle (pt 0 0) (pt 2 0) (pt 1 2)
      let ip = pt 1 1
      t `contains` ip `shouldBe` True

    it "outside" $ do
      let t = makeTriangle (pt 0 0) (pt 2 0) (pt 1 2)
      let ip = pt 2 2
      t `contains` ip `shouldBe` False

    it "on edge" $ do
      let t = makeTriangle (pt 0 0) (pt 2 0) (pt 1 2)
      let ip = pt 1 2
      t `contains` ip `shouldBe` True

    it "on corner" $ do
      let t = makeTriangle (pt 0 0) (pt 2 0) (pt 1 2)
      let ip = pt 0 0
      t `contains` ip `shouldBe` True

