module VectorSpec where

import Dimension
import GeometryClasses
import Point
import Vector

import Test.Hspec (hspec, it, describe, shouldBe)


main :: IO ()
main = hspec spec

spec = do
  it "x (crossProduct)" $ do
    let v0 = makeVector 1 2
    let v1 = makeVector 3 4
    (v0 `x` v1) `shouldBe` (Rz (-2))
    (v1 `x` v0) `shouldBe` (Rz 2)

  it "parallel" $ do
    parallel (makeVector 1 1) (makeVector 1 1) `shouldBe` True
    parallel (makeVector 1 1) (makeVector (-1) (-1)) `shouldBe` True
    parallel (makeVector 1 1) (makeVector 1 2) `shouldBe` False
