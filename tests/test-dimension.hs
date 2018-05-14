module Main where

import Dimension
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

    it "sq (square)" $ do
      sq (Rx 3) `shouldBe` Squared (Rx 9)

  describe "squared" $ do
    it "xSqAdd" $ do
      let x2 = Squared (Rx 2)
      let y2 = Squared (Ry 3)
      x2 `xSqAdd` y2 `shouldBe` Squared (Scalar 5)

  describe "line" $ do
    it "lenSq (squared length)" $ do
      let p0 = Point (Ax 1, Ay 2)
      let p1 = Point (Ax 3, Ay 5)
      let ln = Line (p0, p1)
      lenSq ln `shouldBe` Squared (Length 13)

  describe "circle" $ do
    describe "circlesIntersect" $ do
      describe "overlaps" $ do
        it "horizontal" $ do
          let c0 = Circle (Point (Ax 2, Ay 3), Radius 4)
          let c1 = Circle (Point (Ax 7, Ay 3), Radius 6)
          c0 `circlesIntersect` c1 `shouldBe` True

        it "vertical" $ do
          let c0 = Circle (Point (Ax 2, Ay 3), Radius 4)
          let c1 = Circle (Point (Ax 2, Ay 8), Radius 6)
          c0 `circlesIntersect` c1 `shouldBe` True

        it "angled 345" $ do
          let c0 = Circle (Point (Ax 0, Ay 0), Radius 2)
          let c1 = Circle (Point (Ax 3, Ay 4), Radius 4)
          c0 `circlesIntersect` c1 `shouldBe` True

      describe "touches" $ do
        it "horizontal" $ do
          let c0 = Circle (Point (Ax 2, Ay 3), Radius 4)
          let c1 = Circle (Point (Ax 11, Ay 3), Radius 5)
          c0 `circlesIntersect` c1 `shouldBe` False

        it "vertical" $ do
          let c0 = Circle (Point (Ax 2, Ay 3), Radius 4)
          let c1 = Circle (Point (Ax 2, Ay 12), Radius 5)
          c0 `circlesIntersect` c1 `shouldBe` False

        it "angled 345" $ do
          let c0 = Circle (Point (Ax 0, Ay 0), Radius 2)
          let c1 = Circle (Point (Ax 3, Ay 4), Radius 3)
          c0 `circlesIntersect` c1 `shouldBe` False

      describe "no intersection" $ do
        it "horizontal" $ do
          let c0 = Circle (Point (Ax 2, Ay 3), Radius 4)
          let c1 = Circle (Point (Ax 11, Ay 3), Radius 4.9)
          c0 `circlesIntersect` c1 `shouldBe` False

        it "vertical" $ do
          let c0 = Circle (Point (Ax 2, Ay 3), Radius 4)
          let c1 = Circle (Point (Ax 2, Ay 12), Radius 4.9)
          c0 `circlesIntersect` c1 `shouldBe` False

        it "angled 345" $ do
          let c0 = Circle (Point (Ax 0, Ay 0), Radius 2)
          let c1 = Circle (Point (Ax 3, Ay 4), Radius 2.9)
          c0 `circlesIntersect` c1 `shouldBe` False

      describe "within" $ do
        it "common center point" $ do
          let c0 = Circle (Point (Ax 2, Ay 3), Radius 4)
          let c1 = Circle (Point (Ax 2, Ay 3), Radius 5)
          c0 `circlesIntersect` c1 `shouldBe` True

        it "offset center point" $ do
          let c0 = Circle (Point (Ax 3, Ay 4), Radius 1)
          let c1 = Circle (Point (Ax 2, Ay 3), Radius 5)
          c0 `circlesIntersect` c1 `shouldBe` True
