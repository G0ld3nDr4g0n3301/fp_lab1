module Task24.Map (nthPermutationMap) where

delete :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete y (z : zs)
  | y == z = zs
  | otherwise = z : delete y zs

permutationsHeavyMap :: Eq a => [a] -> [[a]]
permutationsHeavyMap [] = [[]]
permutationsHeavyMap xs = concatMap generatePermutations xs
  where
    generatePermutations x = map (x :) (permutationsHeavyMap (delete x xs))

allPermsMap :: [[Char]]
allPermsMap = permutationsHeavyMap "0123456789"

nthPermutationMap :: Int -> String
nthPermutationMap n = allPermsMap !! (n - 1)
