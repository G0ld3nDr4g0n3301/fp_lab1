module Task24.Recursive (mainNthPermutation) where

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

nthPermutationNonTail :: Int -> String -> String
nthPermutationNonTail index digits = go index digits ""
  where
    go :: Int -> String -> String -> String
    go currentIdx available accumulator
      | null available = ""
      | currentIdx == 0 = available
      | otherwise =
          let n = length available
              fact = factorial (n - 1)

              digitIdx = currentIdx `div` fact

              chosenDigit = available !! digitIdx

              nextIdx = currentIdx `mod` fact

              nextAvailable = take digitIdx available ++ drop (digitIdx + 1) available
           in chosenDigit : go nextIdx nextAvailable accumulator

mainNthPermutation :: Int -> String
mainNthPermutation n = nthPermutationNonTail (n - 1) "0123456789"
