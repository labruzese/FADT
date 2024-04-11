module CSVLoader
(loadCategoryTable,
names,
numCategories,
numEntries,
Bin,
catName,
successCount,
failCount,
count,
bins,
binFromIndex,
successColumn,
CategoryTable()
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

names :: CategoryTable -> [String]
names = map head

numCategories :: CategoryTable -> Int
numCategories = length

numEntries :: CategoryTable -> Int
numEntries = length . tail . head

data Bin = Bin { catName :: String
                , successCount :: Int
                , failCount :: Int
                } deriving (Show)

count :: Bin -> Int
count b = successCount b + failCount b

--Given a column index and category table, return a list of bins
bins :: Int -> CategoryTable -> [Bin]
bins index ct = map snd (toList hashMap)
    where
        hashMap = foldl' insertToHashmap Map.empty kvs

        kvs :: [(String, Bin)]
        kvs = zipWith pairKV (tail $ ct !! index) (tail $ successColumn ct)

        pairKV :: String -> Bool -> (String, Bin)
        pairKV s b = (s, Bin s (bool 1 0 b) (bool 0 1 b))

insertToHashmap :: Map String Bin -> (String, Bin) -> Map String Bin
insertToHashmap m (k,v) = insertWith adder k v m
    where
        adder :: Bin -> Bin -> Bin
        adder new old = Bin (catName new) (successCount new + successCount old) (failCount new + failCount old)

--delete
binFromIndex :: Int -> CategoryTable -> [String]
binFromIndex index = nub . tail . (!! index)

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

main = loadCategoryTable "dtd.csv"