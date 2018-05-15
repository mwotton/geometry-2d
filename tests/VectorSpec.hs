module VectorSpec where


import Point
import Vector

import Test.Hspec


main :: IO ()
main = hspec spec

spec = do
  it "crossProduct" $ do
    let v0 = makeVector 1 2
    let v1 = makeVector 3 4
    (v0 `crossProduct` v1) `shouldBe` (ZVector (-2))
    (v1 `crossProduct` v0) `shouldBe` (ZVector 2)

  it "diffPoints" $ do
    diffPoints (pt 1 2) (pt 3 5) `shouldBe` makeVector 2 3
