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

-- | Just a couple of type alias to help disambiguate between type and improve
-- readability.
type Program = [Int]
type Outputs = [Int]

-- | IP stands for instruction pointer. It just points to the index of the
-- beginning of the next instruction. The instruction opcode determines how many
-- operands there are and thus where the next instruction starts.
type IP = Int

-- | The input for opcode 3
type Input = Int

run :: IO ()
run = do
  text <- B.readFile "../inputs/2019/input-day5.txt"
  let program = fmap fst $ mapMaybe B.readInt $ B.split ',' text
  putStrLn "== Day 5 =="
  putStrLn "Part 1:"
  print $ runProgram 1 program

  putStrLn "Part 2:"
  print $ runProgram 5 program

-- | Runs a int code program and returns a list of outputs given by opcode 4.
-- The given input is supplied to the input opcode 3.
runProgram :: Input -> Program -> Outputs
runProgram _     []      = []
runProgram input program = runST $ do
  memory <- A.newListArray (0, length program - 1) program
  go 0 memory []
 where
  go :: IP -> Memory s -> Outputs -> ST s Outputs
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
        A.writeArray mem inputAddress input
        go (ip + 2) mem outputs
      4 -> do
        let isP1Immediate = instruction `div` 100 `mod` 10 == 1
        p1     <- A.readArray mem (ip + 1)
        output <- if isP1Immediate then pure p1 else A.readArray mem p1
        go (ip + 2) mem (output : outputs)
      5 -> do
        ip' <- jumpWhenP1 (/= 0) ip mem
        go ip' mem outputs
      6 -> do
        ip' <- jumpWhenP1 (== 0) ip mem
        go ip' mem outputs
      7 -> do
        applyBinaryOp (\x y -> if x < y then 1 else 0) ip mem
        go (ip + 4) mem outputs
      8 -> do
        applyBinaryOp (\x y -> if x == y then 1 else 0) ip mem
        go (ip + 4) mem outputs
      99 -> pure $ reverse outputs
      _  -> error $ "Unknown opcode: " <> show opcode

-- | Applies the given 2 parameter function to the first two operand parameters
-- and stores the result in the address of the third. The instruction is fetched
-- to determine the parameter mode.
applyBinaryOp :: (Int -> Int -> Int) -> IP -> Memory s -> ST s ()
applyBinaryOp op ip mem = do
  (a, b)        <- getP1P2Values ip mem
  resultAddress <- A.readArray mem (ip + 3)
  A.writeArray mem resultAddress $ op a b

-- | Returns the instruction pointer to jump to. When the given function returns
-- true, the first parameters value is used otherwise the instruction pointer is
-- just incremented to the next instruction.
jumpWhenP1 :: (Int -> Bool) -> IP -> Memory s -> ST s IP
jumpWhenP1 isP1Jumping ip mem = do
  (a, b) <- getP1P2Values ip mem
  pure $ if isP1Jumping a then b else ip + 3

-- | fetches the instruction to determine the parameter mode, gathers, and
-- returns the first two parameters value.
getP1P2Values :: IP -> Memory s -> ST s (Int, Int)
getP1P2Values ip mem = do
  instruction <- A.readArray mem ip
  let isP1Immediate = instruction `div` 100 `mod` 10 == 1
      isP2Immediate = instruction `div` 1000 `mod` 10 == 1
  p1 <- A.readArray mem (ip + 1)
  p2 <- A.readArray mem (ip + 2)
  a  <- if isP1Immediate then pure p1 else A.readArray mem p1
  b  <- if isP2Immediate then pure p2 else A.readArray mem p2
  pure (a, b)
