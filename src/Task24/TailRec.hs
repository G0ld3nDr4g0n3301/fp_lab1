module Task24.TailRec (mainNthPermutationTailRec) where

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

nthPermutationTailRec :: Int -> String -> String
nthPermutationTailRec index digits = go index digits ""
  where
    go :: Int -> String -> String -> String
    go currentIdx available acc
      | null available = acc
      | currentIdx == 0 = acc ++ available
      | otherwise =
          let n = length available
              fact = factorial (fromIntegral n - 1)

              digitIdx = currentIdx `div` fact

              chosenDigit = available !! digitIdx

              nextIdx = currentIdx `mod` fact

              nextAvailable = take digitIdx available ++ drop (digitIdx + 1) available
           in go nextIdx nextAvailable (acc ++ [chosenDigit])

mainNthPermutationTailRec :: Int -> String
mainNthPermutationTailRec n = nthPermutationTailRec (n - 1) "0123456789"
