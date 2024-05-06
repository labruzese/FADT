module TreeTesting
(main
) where

import CSVLoader
import DecisionTree
import Entropy
import Data.Maybe (fromMaybe)

combinations :: Int -> [[Int]]
combinations n = concatMap (\r -> combination n r [] []) [1..n-1]

combination :: Int -> Int -> [Int] -> [[Int]] -> [[Int]]
combination n r buildingList finishedLists
    | itemsLeftToAdd == 0 = [buildingList]
    | otherwise = finishedLists ++ newAddedLists
    where   
            itemsLeftToAdd = r - length buildingList

            nextLowestNumber = 1 + fromMaybe (-1) (safeHead buildingList)
            newBuildingLists = map (: buildingList) [nextLowestNumber .. (n + 1 - itemsLeftToAdd)]
            newAddedLists = concatMap (\b -> combination n r b []) newBuildingLists

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

--Given an example and decision, checks if the decision matches the example decision
validateMaybeEntry :: Entry -> Maybe Bool -> Maybe Bool
validateMaybeEntry ex Nothing = Nothing
validateMaybeEntry ex (Just b) = Just $ validateEntry ex b

validateEntry :: Entry -> Bool -> Bool
validateEntry entry decision = decision == result entry

testEntry :: Entry -> DecisionTree -> Maybe Bool
testEntry ex dt = validateMaybeEntry ex (makeDecision dt ex)

main :: IO ()
main = do
    --Load Category table
    ct <- loadCategoryTable "FullWithDecision.csv"

    --Build decision tree
    let dt = decisionTree $ keepEntries [0..20] ct

    --Print stuff
    putStr . concat $ replicate 10 "\n"
    printTree dt

    --Pull examples
    let examples = drop 31 (pullEntries ct)
    let tests = map (\ex -> maybe "Nothing" show (testEntry ex dt)) examples
    print tests
    let numCorrect = length (filter (=="True") tests)
    let totalTests = length tests
    putStrLn $ show numCorrect ++ "/" ++ show totalTests ++ " = " ++ show (fromIntegral numCorrect / fromIntegral totalTests)
