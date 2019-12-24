{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

-- | Advent of Code 2019 day 3 solution.
module Day3
  ( Direction(..)
  , Line(..)
  , findIntersection
  , findIntersections
  , manhattanDistance
  , parseWireStr
  , run
  , toCoords
  , snipLoops
  , stepsToIntersection
  , insertWhen
  , isPtOnLine
  )
where

import           Control.Applicative            ( liftA2 )
import qualified Data.ByteString.Char8         as B
import           Data.List.HT                   ( mapAdjacent )
import           Data.Foldable                  ( foldl' )
import           Data.Maybe                     ( mapMaybe )
import           Data.Traversable               ( mapAccumL )
import           Control.Monad                  ( join )
import           Safe.Foldable                  ( minimumMay
                                                , maximumMay
                                                )
import qualified Data.Set                      as Set
import           Data.List.Index                ( indexed )
import qualified Data.List.Extra               as E

type Point = (Int, Int)

data Line = Line Point Point
  deriving (Show, Eq)

data Direction = U | D | L | R deriving (Show, Eq)

run :: IO ()
run = do
  text <- B.readFile "../inputs/2019/input-day3.txt"
  let wireLists = B.lines text
  case wireLists of
    wireStr1 : wireStr2 : _ -> do

      let wireDirections1 = parseWireStr wireStr1
          wireDirections2 = parseWireStr wireStr2
          wireCoords1     = toCoords wireDirections1
          wireCoords2     = toCoords wireDirections2
          wireA           = mapAdjacent Line wireCoords1
          wireB           = mapAdjacent Line wireCoords2
          intersections   = findIntersections wireA wireB

      putStrLn "== Day 3 =="
      putStrLn "Part 1:"
      print $ minimumMay $ manhattanDistance <$> intersections

      putStrLn "Part 2:"

      let
        intersectionsWithinA = Set.fromList $ findIntersections wireA wireA
        intersectionsWithinB = Set.fromList $ findIntersections wireB wireB
        noLoopCoords1        = snipLoops $ foldl'
          (\cs x -> insertWhen x (isPtOnLine x) cs)
          wireCoords1
          intersectionsWithinA
        noLoopCoords2 = snipLoops $ foldl'
          (\cs x -> insertWhen x (isPtOnLine x) cs)
          wireCoords2
          intersectionsWithinB
        stepsPerIntersection1 =
          stepsToIntersection noLoopCoords1 <$> intersections
        stepsPerIntersection2 =
          stepsToIntersection noLoopCoords2 <$> intersections

      print
        $   minimumMay
        $   uncurry (+)
        <$> zip stepsPerIntersection1 stepsPerIntersection2

    _ -> putStrLn "Expected input to have two lists of wire coordinates."

stepsToIntersection :: [Point] -> Point -> Int
stepsToIntersection wireCoords intersection =
  sum
    $ mapAdjacent lineLen
    $ (intersection :)
    $ reverse
    $ takeAdjacentWhile (\a b -> not $ isPtOnLine intersection a b)
    $ (0, 0)
    : wireCoords

-- | Returns the length of a vertical or horizontal line.
lineLen :: Point -> Point -> Int
lineLen (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

toCoords :: [(Direction, Int)] -> [Point]
toCoords directions = snd $ mapAccumL
  (\prev curr -> let c = toCoord prev curr in (c, c))
  (0, 0)
  directions
 where
  toCoord :: Point -> (Direction, Int) -> Point
  toCoord (x, y) (U, n) = (x, y + n)
  toCoord (x, y) (D, n) = (x, y - n)
  toCoord (x, y) (L, n) = (x - n, y)
  toCoord (x, y) (R, n) = (x + n, y)

-- | parses text that looks like: "R18,U42,L33"
-- into: [(R, 18), (U, 42), (L, 33)]
parseWireStr :: B.ByteString -> [(Direction, Int)]
parseWireStr = mapMaybe (toDirectionValue . B.splitAt 1) . B.split ','
 where
  toDirectionValue :: (B.ByteString, B.ByteString) -> Maybe (Direction, Int)
  toDirectionValue (d, n) = liftA2 (,) (toDirection d) (fst <$> B.readInt n)

  toDirection :: B.ByteString -> Maybe Direction
  toDirection "U" = Just U
  toDirection "D" = Just D
  toDirection "L" = Just L
  toDirection "R" = Just R
  toDirection _   = Nothing

manhattanDistance :: Point -> Int
manhattanDistance (x, y) = abs x + abs y

-- | Finds the intersections between two lists of lines
findIntersections :: [Line] -> [Line] -> [Point]
findIntersections wireA wireB =
  mapMaybe (uncurry findIntersection) [ (x, y) | x <- wireA, y <- wireB, x /= y]

-- | Finds the intersection point between two vertical or horizontal line
-- segments.
findIntersection :: Line -> Line -> Maybe Point
findIntersection (Line (x1, y1) (x2, y2)) (Line (x3, y3) (x4, y4))
  | x1 - x2 == 0 && y3 - y4 == 0 && isBetween x1 x3 x4 && isBetween y3 y1 y2
  = Just (x1, y3)
  | x3 - x4 == 0 && y1 - y2 == 0 && isBetween x3 x1 x2 && isBetween y1 y3 y4
  = Just (x3, y1)
  | otherwise
  = Nothing

isPtOnLine :: Point -> Point -> Point -> Bool
isPtOnLine (a, b) (x1, y1) (x2, y2) | x1 == x2 && a == x1 = True
                                    | y1 == y2 && b == y1 = True
                                    | otherwise           = False

-- | Is N on interval (x, y)
isBetween :: Int -> Int -> Int -> Bool
isBetween n x y = n > min x y && n < max x y

takeAdjacentWhile :: (a -> a -> Bool) -> [a] -> [a]
takeAdjacentWhile p (x : y : xs) | p x y     = x : y : takeAdjacentWhile p xs
                                 | otherwise = []
takeAdjacentWhile _ _ = []

snipLoops :: Ord a => [a] -> [a]
snipLoops list =
  let indexedList = indexed list
      getIndicesToRemove =
          ( ( uncurry (liftA2 (\l r -> tail [l .. r]))
            . (\xs -> (minimumMay xs, maximumMay xs))
            )
          . fmap fst
          )
      indicesToRemove =
          Set.fromList
            $ join
            $ mapMaybe getIndicesToRemove
            $ filter (\xs -> length xs > 1)
            $ E.groupOn snd
            $ E.sortOn snd indexedList
  in  snd <$> filter (\(i, _) -> not $ Set.member i indicesToRemove) indexedList

-- | inserts between any two elements when the given predicate is true
insertWhen :: a -> (a -> a -> Bool) -> [a] -> [a]
insertWhen item p list = go list []
 where
  go (x : y : xs) acc | p x y     = go xs (x : item : y : acc)
                      | otherwise = go (y : xs) (x : acc)
  go [x] acc = reverse (x : acc)
  go []  acc = reverse acc
