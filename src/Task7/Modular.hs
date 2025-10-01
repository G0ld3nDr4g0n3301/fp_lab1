module Task7.Modular (nthPrimeMod) where

-- генерация
candidates :: [Int]
candidates = [2 ..]

-- фильтрация
sieve :: [Int] -> [Int]
sieve [] = []
sieve (p : xs) = p : sieve (filter (\x -> x `mod` p /= 0) xs)

-- получение результата
nthPrimeMod :: Int -> Int
nthPrimeMod n = sieve candidates !! (n - 1)
