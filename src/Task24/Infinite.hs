module Task24.Infinite (mainNthPermutationLazy) where

permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations xs = [x : ps | x <- xs, ps <- permutations (delete x xs)]
  where
    delete :: Eq a => a -> [a] -> [a]
    delete _ [] = []
    delete y (z : zs)
      | y == z = zs
      | otherwise = z : delete y zs

mainNthPermutationLazy :: Int -> String
mainNthPermutationLazy n = permutations "0123456789" !! (n - 1)
