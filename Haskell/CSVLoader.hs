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
smallmain
) where

import System.IO
import Data.Map.Strict (insertWith, Map, singleton, toList)
import qualified Data.Map.Strict as Map
import Text.Read

import Data.List
    ( isInfixOf,
      transpose,
      nub,
      foldl',
      minimumBy,
      tails,
      sortBy,
      find )
import Data.Bool
import Distribution.Compat.Prelude (readMaybe)
import Data.Ord (comparing)
import Debug.Trace (trace)
import Helpers ( count, mapIndexes, removeFirst, findIndexInList, findFirstMatchingIndex )
import Data.Sequence (mapWithIndex)

type StringCategoryTable = [StringCategory]
type StringCategory = [String]

data NewCategoryTable = NewCategoryTable { results :: BoolCategory, predictors :: [BoolCategory] } deriving (Show)

data BoolCategory = BoolCategory { question :: String, dataPoints :: CategoryData, isAcceptable :: String -> Bool } deriving (Show)
type CategoryData = [Bool]

data Entry = Entry { result :: Bool, askQuestion :: Map.Map String Bool} deriving (Show)

missing :: String
missing = "N/A"

successColumnHeader :: String
successColumnHeader = "CS Req Grade"

minimumSuccess :: String
minimumSuccess = "B"

numCategories :: NewCategoryTable -> Int
numCategories x = length $ predictors x

numEntries :: NewCategoryTable -> Int
numEntries = length . entries

--Loads the CategoryTable from a file path
loadCategoryTable :: FilePath -> IO NewCategoryTable
loadCategoryTable path = do
    content <- readFile path
    return $ joinToTable . concatMap splitCategory . formatMissing . transpose $ map (splitOn ',') (lines content)
        where
            joinToTable cats = NewCategoryTable  {results = cats !! indexOfSuccessColumn cats, predictors = predictorsColumns cats}
            indexOfSuccessColumn = findFirstMatchingIndex (\x -> question x `isInfixOf` successColumnHeader)
            predictorsColumns cats = take (indexOfSuccessColumn cats) cats ++ drop (indexOfSuccessColumn cats +1) cats

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn delim xs = case break (== delim) xs of
    (before, []) -> [before]
    (before, _:ys) -> before : splitOn delim ys


--      ~Overall Filters~
missingFilter :: String -> String
missingFilter "MISSING" = "N/A"
missingFilter "" = "N/A"
missingFilter str = str

--Applies the missing filter
formatMissing :: StringCategoryTable -> StringCategoryTable
formatMissing = map (map missingFilter)

splitCategory :: StringCategory -> [BoolCategory]
splitCategory category = map buildCategoryAtSplitPoint $ getPossibleSplitPoints (head category) (tail category)
    where
        buildCategoryAtSplitPoint isAcceptableFun = BoolCategory
            (head category)
            (map isAcceptableFun (tail category))
            isAcceptableFun

getPossibleSplitPoints :: String -> StringCategory -> [String -> Bool]
getPossibleSplitPoints name category
    | successColumnHeader `isInfixOf` name = [resultColumnSplit]
    | "Grade" `isInfixOf` name = uniqueFeaturesSortedWith gradeComparator $ filter (/= missing) category
    | "Birth" `isInfixOf` name = uniqueFeaturesSortedWith birthMonthComparator $ filter (/= missing) category
    | "Level" `isInfixOf` name = uniqueFeaturesSortedWith classLevelComparator $ filter (/= missing) category
    | "Tarties" `isInfixOf` name = uniqueFeaturesSortedWith classLevelComparator $ filter (/= missing) category
    | "Siblings" `isInfixOf` name = uniqueFeaturesSortedWith siblingComparator $ filter (/= missing) category
    | otherwise = map (flip elem) (combos $ nub category)
    where
        resultColumnSplit = (<= gradeComparator minimumSuccess) . gradeComparator

        uniqueFeaturesSortedWith = if missing `elem` category then dividedSortedListWithMissing else dividedSortedList

        dividedSortedList func features = split $ nub $ sortBy (comparing func) features
        dividedSortedListWithMissing func features = dividedSortedList func features ++ splitWithMissing (nub $ sortBy (comparing func) features)

        split :: [String] -> [String -> Bool]
        split = map (<=)

        splitWithMissing :: [String] -> [String -> Bool]
        splitWithMissing = map (\ x y -> y <= x || y == missing)

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

gradeComparator :: String -> Double
gradeComparator x = charToDouble (head x) + plusMinusAnalysis x
    where
        plusMinusAnalysis x
            | "+" `isInfixOf` x = -0.33
            | "-" `isInfixOf` x = 0.33
            | otherwise = 0
        charToDouble x
            | x == 'A' = 1.0
            | x == 'B' = 2.0
            | x == 'C' = 3.0
            | x == 'D' = 4.0
            | x == 'F' = 5.0
            | otherwise = 10.0

birthMonthComparator :: String -> Double
birthMonthComparator string =
  let month = read string :: Int
  in fromIntegral ((month + 3) `mod` 12)

classLevelComparator :: String -> Double
classLevelComparator "AP" = 1
classLevelComparator "Honors" = 2
classLevelComparator "Advanced" = 3
classLevelComparator "Adv" = 3
classLevelComparator "Accelerated" = 4
classLevelComparator "Acclerated" = 4
classLevelComparator "Standard" = 5
classLevelComparator "CP" = 6
classLevelComparator x = error $ "unrecognized class level " ++ show x

abstComparator :: String -> Double
abstComparator = read

siblingComparator :: String -> Double
siblingComparator = read

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


trim :: Int -> NewCategoryTable -> NewCategoryTable
trim sp ct = NewCategoryTable {
    results = editDataPoints (results ct) (take sp $ dataPoints $ results ct),
    predictors = map (\x -> editDataPoints x $ take sp (dataPoints x)) (predictors ct)
    }

trim2 :: Int -> NewCategoryTable -> NewCategoryTable
trim2 sp ct = NewCategoryTable {
    results = editDataPoints (results ct) (drop sp $ dataPoints $ results ct),
    predictors = map (\x -> editDataPoints x $ drop sp (dataPoints x)) (predictors ct)
    }

trimCategories :: StringCategoryTable -> StringCategoryTable
trimCategories = take 5

main = loadCategoryTable "49dtd.csv"

smallmain = loadCategoryTable "wikiTest.csv"

-- editDataPoints cat newdps = BoolCategory { question = question cat, dataPoints = newdps, acceptableFeatures = acceptableFeatures cat }
-- editQuestion cat newQ = BoolCategory { question = newQ, dataPoints = dataPoints cat, acceptableFeatures = acceptableFeatures cat }