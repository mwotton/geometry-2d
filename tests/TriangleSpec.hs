module TriangleSpec where

import GeometryClasses
import Dimension
import Point
import Line
import Vector
import Triangle

import Data.List

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

  it "edges" $ do
    let (p0, p1, p2) = (pt 0 0, pt 1 1, pt 0 1)
    let t = makeTriangle p0 p1 p2
    let ls = [p0 ..- p1, p1 ..- p2, p2 ..- p0]
    edges t `shouldBe` ls

  describe "intersects" $ do
    mapM_ test [
        ( "ln Xs 0th seg, pt in"
        , [pt 3 2, pt 3 0, pt 2 0]
        , True
        )
      , ( "2 Xs 2, no interior pts"
        , [pt 0 2, pt 5 2, pt 5 3]
        , True
        )
      , ( "all interior"
        , [pt 3 2, pt 3 3, pt 2 2]
        , True
        )
      , ( "no intersect"
        , [pt 6 6, pt 7 7, pt 6 7]
        , False
        )
      ]
    where test (testName, pts, result) = it testName $ do
            mapM_ testIntersect [ (ps0, ps1) |
                                  ps0 <- permutations pts,
                                  ps1 <- permutations basePts]
            where testIntersect (ps0, ps1) = do
                    let t0 = pts2tri ps0
                    let t1 = pts2tri ps1
                    t0 `intersects` t1 `shouldBe` result
                    t1 `intersects` t0 `shouldBe` result

                  basePts = [(pt 1 1), (pt 4 1), (pt 4 5)]


pts2tri [x, y, z] = makeTriangle x y z
