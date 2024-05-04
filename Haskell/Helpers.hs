module Helpers where
import Data.List (elemIndex)
count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

mapIndexes :: ((Int, b) -> c) -> [b] -> [c]
mapIndexes fun a = zipWith (curry fun) [0..length a - 1] a

filterIndexes :: (Int -> Bool) -> [a] -> [a]
filterIndexes condition list = map fst $ filter (condition . snd) $ zip list [0..length list - 1]

removeFirst :: Eq a => a -> [a] -> [a]
removeFirst _ [] = []
removeFirst x (y:ys)
  | x == y    = ys
  | otherwise = y : removeFirst x ys

findIndexInList :: Eq a => a -> [a] -> Int
findIndexInList x xs =
  case elemIndex x xs of
    Just idx -> idx
    Nothing  -> error "unable to find element in list"

findFirstMatchingIndex :: (a -> Bool) -> [a] -> Int
findFirstMatchingIndex _ [] = error "unable to find element in list"
findFirstMatchingIndex p (x:xs)
    | p x = 0
    | otherwise = 1 + findFirstMatchingIndex p xs