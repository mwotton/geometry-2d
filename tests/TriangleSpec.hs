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
  it "hi" $ do
    1 `shouldBe` 1
  -- describe "triangle contains point" $ do
  --   it "point inside" $ do
  --     let t = makeTriangle (pt 0 0) (pt 2 0) (pt 1 2)
  --     let ip = pt 1 1
  --     t `contains` ip `shouldBe` True

  --   it "point outside" $ do
  --     let t = makeTriangle (pt 0 0) (pt 2 0) (pt 1 2)
  --     let ip = pt 2 2
  --     t `contains` ip `shouldBe` False

  --   it "point on edge" $ do
  --     let t = makeTriangle (pt 0 0) (pt 2 0) (pt 1 2)
  --     let ip = pt 1 2
  --     t `contains` ip `shouldBe` True
