module CSVLoader
(loadCategoryTable,
names,
numCategories,
numEntries,
successColumn,
posExamples,
negExamples,
CategoryTable(),
Category()
) where

import System.IO
import Data.Map.Strict (insertWith, Map, singleton, toList)
import qualified Data.Map.Strict as Map

import Data.List (isInfixOf, transpose, nub, foldl')
import Data.Bool

--An array of categories in the format CategoryTable[category][data point]
    --The first column consists of unique category names
    --The first row consists of either "True" or "False" except for the first itme which is the category name
type CategoryTable = [[String]]
type Category = [String]

names :: CategoryTable -> [String]
names = map head

numCategories :: CategoryTable -> Int
numCategories = length

numEntries :: CategoryTable -> Int
numEntries = length . tail . head

successColumn :: CategoryTable -> [Bool]
successColumn  = map read . head

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
formatMissing = map (map missingFilter . tail)


--      ~Specific Filters~
--Applies specific filters to each category
formatCategories :: CategoryTable -> CategoryTable
formatCategories = map (\cat -> map (filterType (head cat)) (tail cat))

--Gets the specific filters for a category
filterType :: String -> (String -> String)
filterType header
    | "CS Req Grade" `isInfixOf` header = successBool
    | "Grade" `isInfixOf` header = gradeFilter
    -- | "Teacher" `isInfixOf` header = teacherFilter
    -- | "Level" `isInfixOf` header = levelFilter
    -- | "World Language" `isInfixOf` header = wlFilter
    -- | "LP" `isInfixOf` header = lpFilter
    -- | "Birth Month" `isInfixOf` header = dobFilter
    -- | "Siblings" `isInfixOf` header = siblingFilter
    | otherwise = id



successBool :: String -> String
successBool grade
    | "A" `isInfixOf` grade = "True"
    | grade == "B+" = "True"
    | otherwise = "False"

gradeFilter :: String -> String
gradeFilter grade
    | strippedGrade > 'C' = "C or worse"
    | otherwise = [strippedGrade]
    where strippedGrade = head $ grade `removeSuffix` '+' `removeSuffix` '-'

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

main = loadCategoryTable "dtd.csv"