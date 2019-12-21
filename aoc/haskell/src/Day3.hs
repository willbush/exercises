{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

-- | Advent of Code 2019 day 3 solution.
module Day3
  ( run
  , parseWireStr
  , findIntersection
  , Direction(..)
  , toCoords
  , findDistancesOfClosestIntersection
  )
where

import           Control.Applicative            ( liftA2 )
import qualified Data.ByteString.Char8         as B
import           Data.Maybe                     ( mapMaybe )
import           Data.Traversable               ( mapAccumL )
import           Safe.Foldable                  ( minimumMay )

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

      let wireCoords1 = (toCoords . parseWireStr) wireStr1
          wireCoords2 = (toCoords . parseWireStr) wireStr2

      putStrLn "== Day 3 =="
      putStrLn "Part 1:"
      print $ findDistancesOfClosestIntersection wireCoords1 wireCoords2
      putStrLn "Part 2:"
    _ -> putStrLn "Expected input to have two lists of wire coordinates."

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

findDistancesOfClosestIntersection :: [Point] -> [Point] -> Maybe Int
findDistancesOfClosestIntersection wireCoords1 wireCoords2 =
  let lines1 = mapAdjacent Line wireCoords1
      lines2 = mapAdjacent Line wireCoords2

      manhattanDistance :: Point -> Int
      manhattanDistance (x, y) = abs x + abs y
  in  minimumMay $ manhattanDistance <$> mapMaybe
        (\(Line a1 a2, Line b1 b2) -> findIntersection a1 a2 b1 b2)
        [ (x, y) | x <- lines1, y <- lines2 ]

-- | Finds the intersection point between two vertical or horizontal line
-- segments.
findIntersection :: Point -> Point -> Point -> Point -> Maybe Point
findIntersection (x1, y1) (x2, y2) (x3, y3) (x4, y4)
  | x1 - x2 == 0 && y3 - y4 == 0 && isBetween x1 x3 x4 && isBetween y3 y1 y2
  = Just (x1, y3)
  | x3 - x4 == 0 && y1 - y2 == 0 && isBetween x3 x1 x2 && isBetween y1 y3 y4
  = Just (x3, y1)
  | otherwise
  = Nothing

-- | Is N on interval [x, y]
isBetween :: Int -> Int -> Int -> Bool
isBetween n x y = n >= min x y && n <= max x y

-- | This function combines every pair of neighbour elements in a list with a
-- certain function.
mapAdjacent :: (a -> a -> b) -> [a] -> [b]
mapAdjacent f xs = zipWith f xs $ tail xs
