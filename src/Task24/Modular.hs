module Task24.Modular (getNthPermutation) where

-- генерация
factorial :: Int -> Int
factorial 0 = 1
factorial n = product [1 .. n]

buildPermutationStep :: (Int, String, String) -> Int -> (Int, String, String)
buildPermutationStep (currentIdx, availableDigits, accumulator) n =
  let fact = factorial (n - 1)

      digitIdx = currentIdx `div` fact

      chosenDigit = availableDigits !! digitIdx

      nextavailableDigits = take digitIdx availableDigits ++ drop (digitIdx + 1) availableDigits

      nextIdx = currentIdx `mod` fact
   in (nextIdx, nextavailableDigits, accumulator ++ [chosenDigit])

-- свёртка
nthPermutationModular :: Int -> String
nthPermutationModular index =
  let lengths = [10, 9 .. 1]

      initialState = (index, "0123456789", "")

      (_, _, resultPermutation) = foldl' buildPermutationStep initialState lengths
   in resultPermutation

-- получение значения
getNthPermutation :: Int -> String
getNthPermutation n = nthPermutationModular (n - 1)
