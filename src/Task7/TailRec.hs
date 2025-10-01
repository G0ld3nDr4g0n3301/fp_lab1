module Task7.TailRec (nthPrimeTail) where

isPrime :: Int -> Bool
isPrime n
  | n <= 1 = False
  | otherwise = null [x | x <- [2 .. floor (sqrt (fromIntegral n :: Double))], n `mod` x == 0]

nthPrimeTail :: Int -> Int
nthPrimeTail n = findPrime n 0 1
  where
    findPrime :: Int -> Int -> Int -> Int
    findPrime target count current
      | count == target = current - 1
      | isPrime current = findPrime target (count + 1) (current + 1)
      | otherwise = findPrime target count (current + 1)
