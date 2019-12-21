{-# LANGUAGE OverloadedStrings #-}

import           Day1                           ( calcFuel
                                                , calcModuleFuel
                                                )
import           Day2                           ( runProgram )
import           Day3                           ( parseWireStr
                                                , findIntersection
                                                , Direction(..)
                                                , toCoords
                                                , findDistancesOfClosestIntersection
                                                )
import           Test.Hspec

main :: IO ()
main = hspec $ do

  describe "AOC Day 1" $ do
    describe "Part 1 Calc Fuel function" $
      it "should do basic calculations" $ do
        calcFuel 12 `shouldBe` 2
        calcFuel 14 `shouldBe` 2
        calcFuel 1969 `shouldBe` 654
        calcFuel 100756 `shouldBe` 33583
    describe "Part 2 Calc Fuel function" $
      it "should do recursive calculations for the fuel of the fuel" $ do
        calcModuleFuel 12 `shouldBe` 2
        calcModuleFuel 14 `shouldBe` 2
        calcModuleFuel 1969 `shouldBe` 966
        calcModuleFuel 100756 `shouldBe` 50346

  describe "AOC Day 2" $ describe "Part 1 runProgram function" $ do
    it "should run simple programs" $ do
      runProgram [1, 0, 0, 0, 99] `shouldBe` [2, 0, 0, 0, 99]
      runProgram [2, 3, 0, 3, 99] `shouldBe` [2, 3, 0, 6, 99]
      runProgram [2, 4, 4, 5, 99, 0] `shouldBe` [2, 4, 4, 5, 99, 9801]
      runProgram [1, 1, 1, 4, 99, 5, 6, 0, 99]
        `shouldBe` [30, 1, 1, 4, 2, 5, 6, 0, 99]
    it "should halt when 99 is reached" $ do
      let doNothingProgram = [99, 0, 0, 0, 1, 0, 0, 0]
      runProgram doNothingProgram `shouldBe` doNothingProgram

  describe "AOC Day 3" $ do
    describe "Part 1 parse wire function" $
      it "should parse and drop invalid input" $ do
        parseWireStr "" `shouldBe` []
        parseWireStr "R18,U42,L33" `shouldBe` [(R, 18), (U, 42), (L, 33)]
        parseWireStr "R18,X42,L33" `shouldBe` [(R, 18), (L, 33)]
        parseWireStr "R18,UX,L33" `shouldBe` [(R, 18), (L, 33)]
    describe "Part 1 find intersection function" $
      it "finds the intersection point between two line segments on a 2D plane." $ do
        -- ...|....
        -- ...|....
        -- ..-X---.
        -- ...|....
        -- ........
        -- o.......
        -- In the above o represents the origin of a Cartesian plane. Two line
        -- segments intersect at the point X.
        findIntersection (3, 5) (3, 2) (6, 3) (2, 3) `shouldBe` Just (3, 3)
        -- ..........
        -- ......|...
        -- ......|...
        -- ...---X--.
        -- ......|...
        -- ......|...
        -- ..........
        -- ..........
        -- o.........
        -- In the above o represents the origin of a Cartesian plane. Two line
        -- segments intersect at the point X.
        findIntersection (8, 5) (3, 5) (6, 7) (6, 3) `shouldBe` Just (6, 5)
        -- ...........
        -- .-------...
        -- ...........
        -- .........|.
        -- .........|.
        -- .........|.
        -- .........|.
        -- .........|.
        -- .o.......|.
        -- ...........
        -- In the above o represents the origin of a Cartesian plane.
        -- Note the line segments do not intersect.
        findIntersection (8, 0) (8, 5) (0, 7) (6, 7) `shouldBe` Nothing

    describe "Part 1 to coordinates function" $
      it "translates a list of directions and amounts to points in a cartesian plane" $ do
        toCoords [] `shouldBe` []
        toCoords [(U, 1)] `shouldBe` [(0, 1)]
        toCoords [(U, 1), (R, 1)] `shouldBe` [(0, 1), (1, 1)]
        toCoords [(U, 1), (R, 1), (D, 11)] `shouldBe` [(0, 1), (1, 1), (1, -10)]

    describe "Part 1 distances of the closest intersection function" $
      it "finds the distances of the closest intersection to the origin." $ do
        findDistancesOfClosestIntersection
         ((toCoords . parseWireStr) "R8,U5,L5,D3")
         ((toCoords . parseWireStr) "U7,R6,D4,L4") `shouldBe` Just 6
        findDistancesOfClosestIntersection
         ((toCoords . parseWireStr) "R75,D30,R83,U83,L12,D49,R71,U7,L72")
         ((toCoords . parseWireStr) "U62,R66,U55,R34,D71,R55,D58,R83") `shouldBe` Just 159
        findDistancesOfClosestIntersection
         ((toCoords . parseWireStr) "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51")
         ((toCoords . parseWireStr) "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7") `shouldBe` Just 135

