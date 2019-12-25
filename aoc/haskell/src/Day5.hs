-- | Advent of Code 2019 day 5 solution.

module Day5 (run, runProgram) where

import           Control.Monad.ST               ( ST
                                                , runST
                                                )
import qualified Data.Array.ST                 as A
import qualified Data.ByteString.Char8         as B
import           Data.Maybe                     ( mapMaybe )

-- | The memory holds the program state in an unboxed array mutable in the ST
-- monad. Memory is part of the terminology from part 2.
type Memory s = A.STUArray s Int Int

-- | IP stands for instruction pointer. It just points to the index of the
-- beginning of the next instruction. The instruction opcode determines how many
-- operands there are and thus where the next instruction starts.
type IP = Int

run :: IO ()
run = do
  text <- B.readFile "../inputs/2019/input-day5.txt"
  let program = fmap fst $ mapMaybe B.readInt $ B.split ',' text
  putStrLn "== Day 5 =="
  putStrLn "Part 1:"
  print $ runProgram program

  putStrLn "Part 2:"

runProgram :: [Int] -> [Int]
runProgram []      = []
runProgram program = runST $ do
  memory <- A.newListArray (0, length program - 1) program
  compute 0 memory

compute :: IP -> Memory s -> ST s [Int]
compute insPtr memory = go insPtr memory []
 where
  go :: IP -> Memory s -> [Int] -> ST s [Int]
  go ip mem outputs = do
    instruction <- A.readArray mem ip
    let opcode = instruction `mod` 100
    case opcode of
      1 -> do
        applyBinaryOp (+) ip mem
        go (ip + 4) mem outputs
      2 -> do
        applyBinaryOp (*) ip mem
        go (ip + 4) mem outputs
      3 -> do
        inputAddress <- A.readArray mem (ip + 1)
        A.writeArray mem inputAddress 1 -- input is always 1
        go (ip + 2) mem outputs
      4 -> do
        let isP1Immediate = instruction `div` 100 `mod` 10 == 1
        p1     <- A.readArray mem (ip + 1)
        output <- if isP1Immediate then pure p1 else A.readArray mem p1
        go (ip + 2) mem (output : outputs)
      99 -> pure $ reverse outputs
      _  -> error $ "Unknown opcode: " <> show opcode

applyBinaryOp :: (Int -> Int -> Int) -> IP -> Memory s -> ST s ()
applyBinaryOp op ip mem = do
  instruction <- A.readArray mem ip
  let isP1Immediate = instruction `div` 100 `mod` 10 == 1
      isP2Immediate = instruction `div` 1000 `mod` 10 == 1
  p1            <- A.readArray mem (ip + 1)
  p2            <- A.readArray mem (ip + 2)
  resultAddress <- A.readArray mem (ip + 3)
  a             <- if isP1Immediate then pure p1 else A.readArray mem p1
  b             <- if isP2Immediate then pure p2 else A.readArray mem p2
  A.writeArray mem resultAddress $ op a b
