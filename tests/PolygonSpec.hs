module PolygonSpec where
-- module Main where

import Point
import Polygon

import Test.Hspec


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "holder" $ do
    1 `shouldBe` 0
