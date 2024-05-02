module Haskell.Helpers where
count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

mapIndexes :: ((Int, b) -> c) -> [b] -> [c]
mapIndexes fun a = zipWith (curry fun) [0..length a - 1] a