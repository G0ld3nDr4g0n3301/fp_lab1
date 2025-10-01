module Task7.Infinite (nthPrimeInf) where

sieve :: [Int] -> [Int]
sieve [] = []
sieve (p : xs) =
  p
    : sieve [x | x <- xs, x `mod` p /= 0]

primes :: [Int]
primes = sieve [2 ..]

nthPrimeInf :: Int -> Int
nthPrimeInf n = primes !! (n - 1)
