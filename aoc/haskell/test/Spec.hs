{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.List.HT                   ( mapAdjacent )
import           Day1                           ( calcFuel
                                                , calcModuleFuel
                                                )
import           Day2                           ( runProgram )
import           Day3                           ( Direction(..)
                                                , Line(..)
                                                , findIntersection
                                                , findIntersections
                                                , manhattanDistance
                                                , parseWireStr
                                                , toCoords
                                                , snipLoops
                                                , stepsToIntersection
                                                , insertWhen
                                                , isPtOnLine
                                                )
import           Day4                           ( meetsCriteria
                                                , meetsCriteria'
                                                , toDigits
                                                )
import           Test.Hspec
import           Safe.Foldable                  ( minimumMay )
import           Data.Foldable                  ( foldl' )
import qualified Data.ByteString.Char8         as B
import qualified Data.Set                      as Set

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
        findIntersection (Line(3, 5) (3, 2)) (Line (6, 3) (2, 3)) `shouldBe` Just (3, 3)
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
        findIntersection (Line (8, 5) (3, 5)) (Line (6, 7) (6, 3)) `shouldBe` Just (6, 5)

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
        findIntersection (Line (8, 0) (8, 5)) (Line (0, 7) (6, 7)) `shouldBe` Nothing

    describe "Part 1 to coordinates function" $
      it "translates a list of directions and amounts to points in a cartesian plane" $ do
        toCoords [] `shouldBe` []
        toCoords [(U, 1)] `shouldBe` [(0, 1)]
        toCoords [(U, 1), (R, 1)] `shouldBe` [(0, 1), (1, 1)]
        toCoords [(U, 1), (R, 1), (D, 11)] `shouldBe` [(0, 1), (1, 1), (1, -10)]

    describe "Part 1 distances of the closest intersection function" $
      it "finds the distances of the closest intersection to the origin." $ do
        findClosestIntersectionDist
          "R8,U5,L5,D3" "U7,R6,D4,L4" `shouldBe` Just 6
        findClosestIntersectionDist
          "R75,D30,R83,U83,L12,D49,R71,U7,L72"
          "U62,R66,U55,R34,D71,R55,D58,R83" `shouldBe` Just 159
        findClosestIntersectionDist
          "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
          "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7" `shouldBe` Just 135

    describe "insert when function" $ do
      it "inserts between any two elements when the given predicate is true" $ do
        insertWhen (7 :: Int) (==) [] `shouldBe` []
        insertWhen (7 :: Int) (==) [1] `shouldBe` [1]
        insertWhen (7 :: Int) (==) [1, 2, 3] `shouldBe` [1, 2, 3]
        insertWhen (7 :: Int) (==) [1, 2, 2, 3] `shouldBe` [1, 2, 7, 2, 3]
        insertWhen (7 :: Int) (==) [1, 2, 2, 3, 3] `shouldBe` [1, 2, 7, 2, 3, 7, 3]

    describe "Part 2 snip loops function" $ do
      it "it removes loops in the wire" $ do
        snipLoops "" `shouldBe` ""
        snipLoops "a" `shouldBe` "a"
        snipLoops "aba" `shouldBe` "a"
        snipLoops "aarsta" `shouldBe` "a"
        snipLoops "aabbta" `shouldBe` "a"
        snipLoops "abcbdedf" `shouldBe` "abdf"
        snipLoops "abcbad" `shouldBe` "ad"
        snipLoops "abba" `shouldBe` "a"
        snipLoops "abcdcba" `shouldBe` "a"
      it "it does not change the list when no duplicates exists" $
        snipLoops "abcd" `shouldBe` "abcd"

    describe "Part 2 find steps  to intersection function" $
      it "snips loops and counts the steps to an intersection" $ do
        findStepsToIntersection
          "R75,D30,R83,U83,L12,D49,R71,U7,L72"
          "U62,R66,U55,R34,D71,R55,D58,R83" `shouldBe` Just 610
        findStepsToIntersection
          "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
          "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7" `shouldBe` Just 410
  describe "AOC Day 4" $ do
    describe "Part 1 toDigits function" $
      it "turns a number into a list of digits." $ do
        toDigits 1 `shouldBe` [1]
        toDigits 123 `shouldBe` [1, 2, 3]
    describe "Part 1 meetsCriteria function" $ do
      it "returns False for numbers that do not meet criteria." $ do
        meetsCriteria 1 `shouldBe` False -- does not have an adjacent same pair
        meetsCriteria 110 `shouldBe` False -- 0 is decreasing
        meetsCriteria 1213 `shouldBe` False -- does not have an adjacent same pair
      it "returns true if the given number meets the criteria." $ do
        meetsCriteria 11 `shouldBe` True
        meetsCriteria 22 `shouldBe` True
        meetsCriteria 112 `shouldBe` True
        meetsCriteria 11233 `shouldBe` True
        meetsCriteria 12344 `shouldBe` True
    describe "Part 2 meetsCriteria' function" $ do
      it "returns False for numbers that do not meet criteria." $ do
        meetsCriteria' 1 `shouldBe` False -- does not have an adjacent same pair
        meetsCriteria' 110 `shouldBe` False -- 0 is decreasing
        meetsCriteria' 1213 `shouldBe` False -- does not have an adjacent same pair
        meetsCriteria' 123444 `shouldBe` False -- pair is not alone
        meetsCriteria' 444555 `shouldBe` False -- pair is not alone
        meetsCriteria' 4444 `shouldBe` False -- pair is not alone
      it "returns true if the given number meets the criteria." $ do
        meetsCriteria' 11 `shouldBe` True
        meetsCriteria' 22 `shouldBe` True
        meetsCriteria' 112 `shouldBe` True
        meetsCriteria' 11233 `shouldBe` True
        meetsCriteria' 12344 `shouldBe` True
        meetsCriteria' 112233 `shouldBe` True
        meetsCriteria' 111122 `shouldBe` True
        meetsCriteria' 223333 `shouldBe` True

-- | Uses the library functions together to solve part 1 of Day3. I could have
-- put this in the library, but I don't really need it there the intermediate
-- results are used for part 2.
findClosestIntersectionDist :: B.ByteString -> B.ByteString -> Maybe Int
findClosestIntersectionDist wireStr1 wireStr2 =
  let wireA = mapAdjacent Line $ (toCoords . parseWireStr) wireStr1
      wireB = mapAdjacent Line $ (toCoords . parseWireStr) wireStr2
  in  minimumMay $ manhattanDistance <$> findIntersections wireA wireB


-- | Uses the library functions together to solve part 2 of Day3.
findStepsToIntersection :: B.ByteString -> B.ByteString -> Maybe Int
findStepsToIntersection wireStr1 wireStr2 =
  let
    wireDirections1      = parseWireStr wireStr1
    wireDirections2      = parseWireStr wireStr2
    wireCoords1          = snipLoops $ toCoords wireDirections1
    wireCoords2          = snipLoops $ toCoords wireDirections2
    wireA                = mapAdjacent Line wireCoords1
    wireB                = mapAdjacent Line wireCoords2
    intersectionsWithinA = Set.fromList $ findIntersections wireA wireA
    intersectionsWithinB = Set.fromList $ findIntersections wireB wireB
    intersections        = findIntersections wireA wireB
    noLoopCoords1        = snipLoops $ foldl'
      (\cs x -> insertWhen x (isPtOnLine x) cs)
      wireCoords1
      intersectionsWithinA
    noLoopCoords2 = snipLoops $ foldl'
      (\cs x -> insertWhen x (isPtOnLine x) cs)
      wireCoords2
      intersectionsWithinB
    stepsPerIntersection1 = stepsToIntersection noLoopCoords1 <$> intersections
    stepsPerIntersection2 = stepsToIntersection noLoopCoords2 <$> intersections
  in
    minimumMay $ uncurry (+) <$> zip stepsPerIntersection1 stepsPerIntersection2
