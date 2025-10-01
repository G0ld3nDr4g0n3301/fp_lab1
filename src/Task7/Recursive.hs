module Task7.Recursive (nthPrimeRec) where

isPrime :: Int -> Bool
isPrime n
  | n <= 1 = False
  | otherwise = null [x | x <- [2 .. floor (sqrt (fromIntegral n :: Double))], n `mod` x == 0]

primesFrom :: Int -> [Int]
primesFrom k
  | isPrime k = k : primesFrom (k + 1)
  | otherwise = primesFrom (k + 1)

nthPrimeRec :: Int -> Int
nthPrimeRec n = primesFrom 2 !! (n - 1)
