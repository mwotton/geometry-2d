module DimensionSpec where

import GeometryClasses
import Dimension

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "x axis" $ do
    it "add" $ do
      Rx 1 =+= Rx 2 `shouldBe` Rx 3
      Ax 1 .+= Rx 2 `shouldBe` Ax 3
      Rx 1 =+. Ax 2 `shouldBe` Ax 3

    it "subtract" $ do
      (Ax 4) .-. (Ax 3) `shouldBe` Rx 1
      (Rx 5) =-= (Rx 3) `shouldBe` Rx 2

    it "multiply" $ do
      Rx 2 =*= Rx 3 `shouldBe` Squared (Rx 6)

    it "sq (square)" $ do
      sq (Rx 3) `shouldBe` Squared (Rx 9)

    -- describe "AbsoluteOps" $ do


  describe "squared" $ do
    it "xSqAdd" $ do
      let x2 = Squared (Rx 2)
      let y2 = Squared (Ry 3)
      x2 `xSqAdd` y2 `shouldBe` Squared (Scalar 5)

  describe "Range" $ do
    it "newRange" $ do
      newRange (Ax 1) (Ax 2) `shouldBe` Range (Ax 1, Ax 2)
      newRange (Ax 2) (Ax 1) `shouldBe` Range (Ax 1, Ax 2)

    describe "intersects" $ do
      it "partial partial" $ do
        let r0 = newRange (Ax 0) (Ax 2)
        let r1 = newRange (Ax 1) (Ax 3)
        intersects r0 r1 `shouldBe` True
        intersects r1 r0 `shouldBe` True

      it "partial total" $ do
        let r0 = newRange (Ax 0) (Ax 3)
        let r1 = newRange (Ax 1) (Ax 2)
        intersects r0 r1 `shouldBe` True
        intersects r1 r0 `shouldBe` True

      it "end point" $ do
        let r0 = newRange (Ax 0) (Ax 1)
        let r1 = newRange (Ax 1) (Ax 2)
        let r2 = newRange (Ax 2) (Ax 3)
        intersects r0 r1 `shouldBe` True
        intersects r1 r0 `shouldBe` True
        intersects r2 r1 `shouldBe` True
        intersects r1 r2 `shouldBe` True

      it "none" $ do
        let r0 = newRange (Ax 0) (Ax 1)
        let r1 = newRange (Ax 2) (Ax 3)
        intersects r0 r1 `shouldBe` False
        intersects r1 r0 `shouldBe` False