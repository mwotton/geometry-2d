module RectangleSpec where

import GeometryClasses
import Vector
import Point
import Rectangle

import Test.Hspec


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "RectVec" $ do
    it "area" $ do
      let r = RectVec (makeVector (-2) 6)
      area r `shouldBe` Area 12

  describe "Rect2P" $ do
    describe "intersects" $ do
      let cases = [True, False]
      mapM_ test [
          ( "corner intesect"
          , Rect2P (pt 0 0, pt 1 1)
          , True
          )
        , ( "edge intersect"
          , Rect2P (pt 5 5, pt 3 4)
          , True
          )
        , ( "area intersect"
          , Rect2P (pt 2 0, pt 3 2)
          , True
          )
        , ( "contained intersect"
          , Rect2P (pt 2 2, pt 3 3)
          , True
          )
        , ( "no intersect"
          , Rect2P (pt 5 5, pt 6 6)
          , False
          )
        ]
      where test (testName, rectangle, result) = it testName $ do
              let rectangle2 = Rect2P (pt 1 1, pt 4 4)
              rectangle `intersects` rectangle2 `shouldBe` result
              rectangle2 `intersects` rectangle `shouldBe` result

  it "mbb (minimum bounding box)" $ do
    mbb [pt 1 1, pt 2 2, pt 3 1, pt 0 1] `shouldBe` Rect2P (pt 0 1, pt 3 2)

