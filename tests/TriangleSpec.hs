module TriangleSpec where

import Dimension
import Point
import Line
import Triangle

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "triangleFromPoints" $ do
    let p0 = pt 0 0
    let p1 = pt 1 1
    let p2 = pt 1 2
    let t = triangleFromPoints p0 p1 p2
    let tRef = Triangle (Line (p0, p1), Line (p1, p2), Line (p2, p0))
    t `shouldBe` tRef

  describe "triangle contains point" $ do
    it "point inside" $ do
      let t = triangleFromPoints (pt 0 0) (pt 2 0) (pt 1 2)
      let ip = pt 1 1
      t `contains` ip `shouldBe` True

    it "point outside" $ do
      let t = triangleFromPoints (pt 0 0) (pt 2 0) (pt 1 2)
      let ip = pt 2 2
      t `contains` ip `shouldBe` False

    it "point on edge" $ do
      let t = triangleFromPoints (pt 0 0) (pt 2 0) (pt 1 2)
      let ip = pt 1 2
      t `contains` ip `shouldBe` True
