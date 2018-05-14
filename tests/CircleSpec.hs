module CircleSpec where

import Dimension
import Point
import Circle

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
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
