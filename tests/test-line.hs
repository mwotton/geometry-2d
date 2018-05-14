module Main where

import Dimension
import Test.Hspec

main = hspec $ do
  describe "line" $ do
    it "lenSq (squared length)" $ do
      let p0 = Point (Ax 1, Ay 2)
      let p1 = Point (Ax 3, Ay 5)
      let ln = Line (p0, p1)
      lenSq ln `shouldBe` Squared (Length 13)