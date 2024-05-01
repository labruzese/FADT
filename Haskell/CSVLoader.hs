module CSVLoader
(loadCategoryTable,
numCategories,
numEntries,
successColumn,
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

import Data.List (isInfixOf, transpose, nub, foldl', minimumBy, tails)
import Data.Bool
import Distribution.Compat.Prelude (readMaybe)
import Data.Ord (comparing)
import Data.List (sortBy)
import Debug.Trace (trace)

--An array of categories in the format CategoryTable[category][data point]
    --The first column consists of unique category names
    --The first row consists of either "True" or "False" except for the first itme which is the category name
type StringCategoryTable = [StringCategory]
type StringCategory = [String]

data NewCategoryTable = NewCategoryTable { predictionColumn :: BoolCategory, predictors :: [BoolCategory] }

data BoolCategory = BoolCategory { name :: String, dataPoints :: CategoryData }
type CategoryData = [Maybe Bool] -- make missing part of the features

missing :: String
missing = "N/A"

numCategories :: StringCategoryTable -> Int
numCategories x = length x - 1

numEntries :: StringCategoryTable -> Int
numEntries = length . entries

successColumn :: StringCategoryTable -> [Bool]
successColumn  = map read . tail . head

--Loads the CategoryTable from a file path
loadCategoryTable :: FilePath -> IO StringCategoryTable
loadCategoryTable path = do
    content <- readFile path
    return $ formatCategories . formatMissing . transpose $ map (splitOn ',') (lines content)

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
        buildCategoryAtSplitPoint acceptableFeatures = BoolCategory (head category) $ map (\x -> Just $ x `elem` acceptableFeatures) (tail category)

getPossibleSplitPoints :: String -> StringCategory -> [StringCategory]
getPossibleSplitPoints name category
    | "Grade" `isInfixOf` name = uniqueFeaturesSortedWith gradeValueOf category
    --ADD OTHER SORTABLE STUFF HERE
    | otherwise = combos $ nub category
    where
        uniqueFeaturesSortedWith func features = split $ nub $ sortBy (comparing func) features
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
--Applies specific filters to each category
formatCategories :: StringCategoryTable -> StringCategoryTable
formatCategories = map (\cat -> head cat : map (filterType (head cat)) (tail cat))

--Gets the specific filters for a category
filterType :: String -> (String -> String)
filterType header
    | "CS Req Grade" `isInfixOf` header = successBool
    | "Grade" `isInfixOf` header = gradeFilter
    | "Teacher" `isInfixOf` header = teacherFilter
    -- | "Level" `isInfixOf` header = levelFilter
    -- | "World Language" `isInfixOf` header = wlFilter
    -- | "LP" `isInfixOf` header = lpFilter
    | "Abs+Tardies" `isInfixOf` header = abstFilter
    | "Birth Month" `isInfixOf` header = birthMonthFilter
    | "Siblings" `isInfixOf` header = siblingFilter
    | otherwise = id

successBool :: String -> String
successBool grade
    | "A" `isInfixOf` grade = "True"
    | grade == "B+" = "True"
    | otherwise = "False"

gradeFilter :: String -> String
gradeFilter grade
    | null grade = missing
    | grade == missing = missing
    | strippedGrade >= 'C' = "C or worse"
    | otherwise = [strippedGrade]
    where strippedGrade = head grade

--given a split point returns the grades binned into stuff better and worse than it
gradeFilterWithSplitPoint :: String -> String -> String
gradeFilterWithSplitPoint sp grade
    | null grade = missing
    | grade == missing = missing
    | gradeComparator grade >= gradeComparator sp = sp ++ " or worse"
    | otherwise = sp ++ " or better"

--find the split point that puts the most successes on one side of the split and returns that feature
findSplitPointGrades :: StringCategoryTable -> StringCategory -> String
findSplitPointGrades ct cat = sortedItems !! minimumBy (comparing (successCountComparator . successCountForFirstFeature . binCategory)) possibleSplitPoints
    where
        --the categories which are grades that are sorted
        sortedItems = nub $ sortBy (comparing gradeComparator) cat

        --All the indexes of possible split points
        possibleSplitPoints = [1..length sortedItems-1]

        --best distance from all successes or all failures
        successCountComparator x = min x (length cat - x)

        --Returns the category with the grade filter at the split point applied
        binCategory sp = map (gradeFilterWithSplitPoint (sortedItems !! sp)) (tail cat)

        --Counts the amount of successes for the first item in the category
        successCountForFirstFeature spCat = count True $ zipWith (isSuccess (head spCat)) spCat (successColumn ct)

        isSuccess bin1 spV scV = (scV && spV == bin1) || (not scV && (spV /= bin1))

        count x = length . filter (== x)

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

teacherFilter :: String -> String
teacherFilter = take 4

abstFilter :: String -> String
abstFilter count =
        case readMaybe count of
        Nothing -> missing
        Just m ->
            if m < 10
                then "Few"
            else if m > 10
                then "Many"
            else missing

birthMonthFilter :: String -> String
birthMonthFilter month =
    case readMaybe month of
        Nothing -> missing
        Just m ->
            if m `elem` [9..12]
                then "Early"
            else if m `elem` [1..4]
                then "Middle"
            else if m `elem` [5..8]
                then "Late"
            else missing

siblingFilter :: String -> String
siblingFilter "0" = "0"
siblingFilter "1" = "1"
siblingFilter _ = "2+"

--Removes the given suffix if found
removeSuffix :: String -> Char -> String
removeSuffix [] _ = []
removeSuffix str c
    | c == last str = init str
    | otherwise = str

posExamples :: StringCategoryTable -> Int
posExamples ct = count True $ successColumn ct
    where
        count x = length . filter (== x)

negExamples :: StringCategoryTable -> Int
negExamples ct = count False $ successColumn ct
    where
        count x = length . filter (== x)

headers :: StringCategoryTable -> [String]
headers = map head

entries :: StringCategoryTable -> [[String]]
entries = transpose . map tail


trim :: Int -> StringCategoryTable -> StringCategoryTable
trim sp x = transpose $ map (transpose x !!) [0..sp]
trim2 :: Int -> StringCategoryTable -> StringCategoryTable
trim2 sp x = transpose $ map (transpose x !!) (0 : [sp+1..numEntries x])


main = loadCategoryTable "49dtd.csv"

loadTrainingSet = loadCategoryTable "wikiTest.csv"

