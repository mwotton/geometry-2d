module Main where

import Distance
import Test.Hspec

main = hspec $ do
  describe "x axis" $ do
    it "add" $ do
      Rx 1 =+= Rx 2 `shouldBe` Rx 3
      Ax 1 .+= Rx 2 `shouldBe` Ax 3
      Rx 1 =+. Ax 2 `shouldBe` Ax 3

    it "subtract" $ do
      (Ax 4) .-. (Ax 3) `shouldBe` Rx 1

    it "multiply" $ do
      Rx 2 =*= Rx 3 `shouldBe` Squared (Rx 6)

  describe "squared" $ do
    it "xSqAdd" $ do
      let x2 = Squared (Rx 2)
      let y2 = Squared (Ry 3)
      x2 `xSqAdd` y2 `shouldBe` Squared (Scalar 5)

  describe "line" $ do
    it "lenSq (squared length)" $ do
      let p0 = Pt (Ax 1, Ay 2)
      let p1 = Pt (Ax 3, Ay 5)
      let ln = Ln (p0, p1)
      lenSq ln `shouldBe` Squared (Length 13)