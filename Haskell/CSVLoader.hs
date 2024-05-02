module CSVLoader
(loadCategoryTable,
numCategories,
numEntries,
posExamples,
negExamples,
StringCategoryTable(),
StringCategory(),
main,
trim,
trim2,
headers,
entries,
loadTrainingSet
) where

import System.IO
import Data.Map.Strict (insertWith, Map, singleton, toList)
import qualified Data.Map.Strict as Map

import Data.List
    ( isInfixOf, transpose, nub, foldl', minimumBy, tails, sortBy )
import Data.Bool
import Distribution.Compat.Prelude (readMaybe)
import Data.Ord (comparing)
import Debug.Trace (trace)
import Haskell.Helpers
import qualified Data.Map.Strict as Map
import Data.Sequence (mapWithIndex)

--An array of categories in the format CategoryTable[category][data point]
    --The first column consists of unique category names
    --The first row consists of either "True" or "False" except for the first itme which is the category name
type StringCategoryTable = [StringCategory]
type StringCategory = [String]

data NewCategoryTable = NewCategoryTable { results :: BoolCategory, predictors :: [BoolCategory] }

data BoolCategory = BoolCategory { question :: String, dataPoints :: CategoryData }
type CategoryData = [Bool] -- make missing part of the features

data Entry = Entry { result :: Bool, askQuestion :: Map.Map String Bool}

missing :: String
missing = "N/A"

numCategories :: NewCategoryTable -> Int
numCategories x = length predictors

numEntries :: NewCategoryTable -> Int
numEntries = length . entries

--Loads the CategoryTable from a file path
loadCategoryTable :: FilePath -> IO NewCategoryTable
loadCategoryTable path = do
    content <- readFile path
    return $ joinToTable . concatMap splitCategory . formatMissing . transpose $ map (splitOn ',') (lines content)
        where joinToTable cats = NewCategoryTable (head cats) (tail cats)

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn delim xs = case break (== delim) xs of
    (before, []) -> [before]
    (before, _:ys) -> before : splitOn delim ys


--      ~Overall Filters~
missingFilter :: String -> String
missingFilter "MISSING" = "N/A"
missingFilter str = str

--Applies the missing filter
formatMissing :: StringCategoryTable -> StringCategoryTable
formatMissing = map (map missingFilter)

splitCategory :: StringCategory -> [BoolCategory]
splitCategory category = map buildCategoryAtSplitPoint $ getPossibleSplitPoints (head category) (tail category)
    where
        buildCategoryAtSplitPoint acceptableFeatures = BoolCategory (newHeader acceptableFeatures) $ map (`elem` acceptableFeatures) (tail category)
        newHeader acceptableFeatures
            | missing `elem` acceptableFeatures = head category ++ show acceptableFeatures

getPossibleSplitPoints :: String -> StringCategory -> [StringCategory]
getPossibleSplitPoints name category
    | "UNMATCHABLE EXAMPLE" `isInfixOf` name = uniqueFeaturesSortedWith gradeValueOf $ filter (/= missing) category
    --ADD OTHER SORTABLE STUFF HERE
    | otherwise = combos $ nub category
    where
        uniqueFeaturesSortedWith func features = if missing `elem` features
            then dividedSortedList func features ++ map (++[missing]) (dividedSortedList func features)
            else dividedSortedList func features

        dividedSortedList func features = split $ nub $ sortBy (comparing func) features

        split xs = map (`take` xs) [1..length xs - 1]

combos :: [a] -> [[a]]
combos list = concatMap specialPick [1..ceiling $ (fromIntegral (length list) - 0.4) / 2.0]
    where
        specialPick n
            | n == halfListLength = take n $ pickFrom list n
            | otherwise = pickFrom list n
        halfListLength = round (fromIntegral (length list) / 2)

pickFrom :: [a] -> Int -> [[a]]
pickFrom _ 0  = [[]]
pickFrom [] _ = []
pickFrom (x:xs) n  = map (x:) (pickFrom xs (n-1)) ++ pickFrom xs n


gradeValueOf :: String -> Double
gradeValueOf x = fromIntegral $ length x
--      ~Specific Filters~

--double value for a grade for comparision
gradeComparator :: String -> Double
gradeComparator x = charToDouble (head x) + plusMinusAnalysis x
    where
        plusMinusAnalysis x
            | "+" `isInfixOf` x = -0.5
            | "-" `isInfixOf` x = 0.5
        charToDouble x
            | x == 'A' = 1.0
            | x == 'B' = 2.0
            | x == 'C' = 3.0
            | x == 'D' = 4.0
            | x == 'F' = 5.0
            | otherwise = 10.0

posExamples :: NewCategoryTable -> Int
posExamples ct = count True $ dataPoints $ results ct

negExamples :: NewCategoryTable -> Int
negExamples ct = count False $ dataPoints $ results ct

headers :: StringCategoryTable -> [String]
headers = map head

entries :: NewCategoryTable -> [Entry]
entries ct = mapIndexes (\(entryId, entry) -> buildEntry (dataPoints (results ct) !! entryId) entry) $ transpose $  
    map (\category -> map (question category,) (dataPoints category)) (predictors ct)
    where
        buildEntry result qAndA = Entry result $ Map.fromList qAndA


trim :: Int -> StringCategoryTable -> StringCategoryTable
trim sp x = transpose $ map (transpose x !!) [0..sp]
trim2 :: Int -> StringCategoryTable -> StringCategoryTable
trim2 sp x = transpose $ map (transpose x !!) (0 : [sp+1..numEntries x])


main = loadCategoryTable "49dtd.csv"

loadTrainingSet = loadCategoryTable "wikiTest.csv"

