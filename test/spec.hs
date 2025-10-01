import Task24.Infinite
import Task24.Map
import Task24.Modular
import Task24.Recursive
import Task24.TailRec
import Task7.Infinite
import Task7.Modular
import Task7.Recursive
import Task7.TailRec
import Test.Hspec

task7Answer :: Int
task7Answer = 104743

task24Answer :: String
task24Answer = "2783915460"

main :: IO ()
main = hspec $ do
  -- task 7

  describe "Task 7 Infinite list" $ do
    it "nthPrimeInf 10001 == 104743" $ do
      nthPrimeInf 10001 `shouldBe` task7Answer

  describe "Task 7 Tail recursion" $ do
    it "nthPrimeTail 10001 == 104743" $ do
      nthPrimeTail 10001 `shouldBe` task7Answer

  describe "Task 7 Recursive" $ do
    it "nthPrimeRec 10001 == 104743" $ do
      nthPrimeRec 10001 `shouldBe` task7Answer

  describe "Task 7 Module" $ do
    it "nthPrimeMod 10001 == 104743" $ do
      nthPrimeMod 10001 `shouldBe` task7Answer

  -- task 24

  describe "Task 24 Infinite list" $ do
    it "mainNthPermutationLazy 1000000 == 2783915460" $ do
      mainNthPermutationLazy 1000000 `shouldBe` task24Answer

  describe "Task 24 Tail recursion" $ do
    it "mainNthPermutationTailRec 1000000 == 2783915460" $ do
      mainNthPermutationTailRec 1000000 `shouldBe` task24Answer

  describe "Task 24 Recursive" $ do
    it "mainNthPermutation 1000000 == 2783915460" $ do
      mainNthPermutation 1000000 `shouldBe` task24Answer

  describe "Task 24 Module" $ do
    it "getNthPermutation 1000000 == 2783915460" $ do
      getNthPermutation 1000000 `shouldBe` task24Answer

  describe "Task 24 Map" $ do
    it "nthPermutationMap 1000000 == 2783915460" $ do
      nthPermutationMap 1000000 `shouldBe` task24Answer
