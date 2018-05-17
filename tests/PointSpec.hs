module PointSpec where

import Dimension
import Vector
import Point

import Test.Hspec

main :: IO ()
main = hspec spec

spec = do
  it "pt (creates point)" $ do
    pt 1 2 `shouldBe` Point (Ax 1, Ay 2)

  it "diffPoints" $ do
    diffPoints (pt 1 2) (pt 3 5) `shouldBe` makeVector 2 3
    (pt 1 2) .->. (pt 3 5) `shouldBe` makeVector 2 3
    (pt 1 2) .<-. (pt 3 5) `shouldBe` makeVector (-2) (-3)

  it "pointPlusVector" $ do
    (pt 2 3) .+-> (makeVector 4 6) `shouldBe` (pt 6 9)