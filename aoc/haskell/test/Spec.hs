import           Day1                           ( calcFuel
                                                , calcModuleFuel
                                                )
import           Day2                           ( runProgram )
import           Test.Hspec

main :: IO ()
main = hspec $ do

  describe "AOC Day 1" $ do
    describe "Part 1 Calc Fuel function"
      $ it "should do basic calculations"
      $ do
          calcFuel 12 `shouldBe` 2
          calcFuel 14 `shouldBe` 2
          calcFuel 1969 `shouldBe` 654
          calcFuel 100756 `shouldBe` 33583
    describe "Part 2 Calc Fuel function"
      $ it "should do recursive calculations for the fuel of the fuel"
      $ do
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
