module CSVLoader
(loadCategoryTable,
names,
numCategories,
numEntries,
successColumn,
posExamples,
negExamples,
CategoryTable(),
Category(),
main,
trim,
trim2
) where

import System.IO
import Data.Map.Strict (insertWith, Map, singleton, toList)
import qualified Data.Map.Strict as Map

import Data.List (isInfixOf, transpose, nub, foldl')
import Data.Bool
import Distribution.Compat.Prelude (readMaybe)

--An array of categories in the format CategoryTable[category][data point]
    --The first column consists of unique category names
    --The first row consists of either "True" or "False" except for the first itme which is the category name
type CategoryTable = [[String]]
type Category = [String]
missing = "N/A"

names :: CategoryTable -> [String]
names = map head

numCategories :: CategoryTable -> Int
numCategories x = length x - 1

numEntries :: CategoryTable -> Int
numEntries = length . tail . head

successColumn :: CategoryTable -> [Bool]
successColumn  = map read . tail . head

--Loads the CategoryTable from a file path
loadCategoryTable :: FilePath -> IO CategoryTable
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
formatMissing :: CategoryTable -> CategoryTable
formatMissing = map (map missingFilter)


--      ~Specific Filters~
--Applies specific filters to each category
formatCategories :: CategoryTable -> CategoryTable
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
    | strippedGrade >= 'C' = "C or worse"
    | otherwise = [strippedGrade]
    where strippedGrade = head grade

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

posExamples :: CategoryTable -> Int
posExamples ct = count True $ successColumn ct
    where
        count x = length . filter (== x)

negExamples :: CategoryTable -> Int
negExamples ct = count False $ successColumn ct
    where
        count x = length . filter (== x)


trim :: CategoryTable -> CategoryTable
trim x = transpose $ map (transpose x !!) [0..30]
trim2 :: CategoryTable -> CategoryTable
trim2 x = transpose $ map (transpose x !!) [31..length x - 1]


main = loadCategoryTable "49dtd.csv"

