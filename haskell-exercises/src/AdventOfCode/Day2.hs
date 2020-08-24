-- | Advent of Code 2019 day 2 solution. I'm being kinda lazy with the error
-- handling in this solution because I'm using 'error' and partial functions.

module AdventOfCode.Day2
  ( runProgram
  , readProgram
  , part1Solution
  , part2Solution
  )
where

import           Control.Monad.ST               ( ST )
import qualified Data.Array.ST                 as A
import           Data.Array.Unboxed             ( elems )
import qualified Data.ByteString.Char8         as B
import           Data.Maybe                     ( mapMaybe )
import           Data.Foldable                  ( find )
import           Safe                           ( headMay )

-- | The memory holds the program state in an unboxed array mutable in the ST
-- monad. Memory is part of the terminology from part 2.
type Memory s = A.STUArray s Int Int

-- | An address is really just an index into the Memory array. Address is part
-- of the terminology from part 2.
type OpCodeAddress = Int

-- | The noun is the value of the program at position (or address) 1. Its
-- possible values range between [0, 99].
type Noun = Int

-- | The verb is the value of the program at position (or address) 2. Its
-- possible values range between [0, 99].
type Verb = Int

part1Solution :: [Int] -> Maybe Int
part1Solution program = headMay $ runProgram $ setNounAndVerb 12 2 program

part2Solution :: [Int] -> Maybe Int
part2Solution program = do
  -- "determine what pair of inputs produces the output 19690720... the output
  -- is available at address 0."
  let allPossibleNounVerbs = [ (n, v) | n <- [0 .. 99], v <- [0 .. 99] ]
      maybeAnswer :: Maybe ((Noun, Verb), Int)
      maybeAnswer = find (\x -> snd x == 19690720) $ fmap
        (\nv@(n, v) -> (nv, head $ runProgram $ setNounAndVerb n v program))
        allPossibleNounVerbs
  -- "What is 100 * noun + verb?"
  fmap (\x -> let nv = fst x in 100 * fst nv + snd nv) maybeAnswer

setNounAndVerb :: Noun -> Verb -> [Int] -> [Int]
setNounAndVerb n v (x : _ : _ : xs) = x : n : v : xs
setNounAndVerb _ _ xs               = xs

runProgram :: [Int] -> [Int]
runProgram program = elems $ A.runSTUArray $ do
  let end = length program - 1
  memory <- A.newListArray (0, end) program
  compute [0, 4 .. end] memory
  pure memory

compute :: [OpCodeAddress] -> Memory s -> ST s ()
compute []                          _   = pure ()
compute (opcodeAddress : addresses) mem = do
  opcode <- A.readArray mem opcodeAddress
  case opcode of
    1 -> do
      applyOp (+) opcodeAddress mem
      compute addresses mem
    2 -> do
      applyOp (*) opcodeAddress mem
      compute addresses mem
    99 -> pure ()
    _  -> error $ "Unknown opcode: " <> show opcode

applyOp :: (Int -> Int -> Int) -> OpCodeAddress -> Memory s -> ST s ()
applyOp op address mem = do
  aAddress      <- A.readArray mem (address + 1)
  bAddress      <- A.readArray mem (address + 2)
  resultAddress <- A.readArray mem (address + 3)
  a             <- A.readArray mem aAddress
  b             <- A.readArray mem bAddress
  A.writeArray mem resultAddress $ op a b

-- | read and parse the program from input file.
readProgram :: IO [Int]
readProgram = do
  text <- B.readFile "./inputs/aoc/2019/input-day2.txt"
  pure $ fmap fst $ mapMaybe B.readInt $ B.split ',' text
