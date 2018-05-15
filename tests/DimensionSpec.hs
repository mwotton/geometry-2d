module DimensionSpec where

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

  -- it "Absolute Multiplication" $ do
  --   Ax 2 .*. Ay 3 `shouldBe` Az 6

  describe "squared" $ do
    it "xSqAdd" $ do
      let x2 = Squared (Rx 2)
      let y2 = Squared (Ry 3)
      x2 `xSqAdd` y2 `shouldBe` Squared (Scalar 5)
