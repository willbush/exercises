-- | Advent of Code 2019 day 2 solution.

module Day2 (run, runProgram) where

import           Control.Monad.ST               ( ST )
import qualified Data.Array.ST                 as A
import           Data.Array.Unboxed             ( elems )
import qualified Data.ByteString.Char8         as B
import           Data.Maybe                     ( mapMaybe )

run :: IO ()
run = do
  text <- B.readFile "../inputs/2019/input-day2.txt"
  let program = fmap fst $ mapMaybe B.readInt $ B.split ',' text
  print $ runProgram $ restoreProgramState program

restoreProgramState :: [Int] -> [Int]
restoreProgramState (x : _ : _ : xs) = x : 12 : 2 : xs
restoreProgramState xs               = xs

runProgram :: [Int] -> [Int]
runProgram program = elems $ A.runSTUArray $ do
  let end = length program - 1
  mutableProgram <- A.newListArray (0, end) program
  compute_ [0, 4 .. end] mutableProgram
  pure mutableProgram

type OpCodeIndex = Int

compute_ :: [OpCodeIndex] -> A.STUArray s Int Int -> ST s ()
compute_ []                      _       = pure ()
compute_ (opcodeIndex : indices) program = do
  opcode <- A.readArray program opcodeIndex
  case opcode of
    1 -> do
      applyOp_ (+) opcodeIndex program
      compute_ indices program
    2 -> do
      applyOp_ (*) opcodeIndex program
      compute_ indices program
    _ -> pure ()

applyOp_
  :: (Int -> Int -> Int) -> OpCodeIndex -> A.STUArray s Int Int -> ST s ()
applyOp_ op i program = do
  aIndex      <- A.readArray program (i + 1)
  bIndex      <- A.readArray program (i + 2)
  resultIndex <- A.readArray program (i + 3)
  a           <- A.readArray program aIndex
  b           <- A.readArray program bIndex
  A.writeArray program resultIndex $ op a b
