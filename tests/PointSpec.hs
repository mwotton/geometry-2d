module PointSpec where

import Dimension
import Point

import Test.Hspec

main :: IO ()
main = hspec spec

spec = do
  it "pt creates point" $ do
    pt 1 2 `shouldBe` Point (Ax 1, Ay 2)
