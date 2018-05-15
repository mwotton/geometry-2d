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
  describe "triangle contains point" $ do
    describe "ccw triangle with point" $ do
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

    describe "cw triangle with point" $ do
      it "inside" $ do
        let t = makeTriangle (pt 0 0) (pt 1 2) (pt 2 0)
        let ip = pt 1 1
        t `contains` ip `shouldBe` True

      it "outside" $ do
        let t = makeTriangle (pt 0 0) (pt 1 2) (pt 2 0)
        let ip = pt 2 2
        t `contains` ip `shouldBe` False

      it "on edge" $ do
        let t = makeTriangle (pt 0 0) (pt 1 2) (pt 2 0)
        let ip = pt 1 2
        t `contains` ip `shouldBe` True

      it "on corner" $ do
        let t = makeTriangle (pt 0 0) (pt 1 2) (pt 2 0)
        let ip = pt 2 0
        t `contains` ip `shouldBe` True
