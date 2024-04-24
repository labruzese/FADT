module CSVLoader
(loadCategoryTable,
names,
name,
numCategories,
numEntries,
removeCategory,
keepEntries,
removeEntries,
successColumn,
tableValue,
Bin,
entries,
response,
successCount,
failCount,
count,
bins,
CategoryTable()
) where

import System.IO
import Data.List
import Data.Map.Strict (insertWith, Map, singleton, toList)
import qualified Data.Map.Strict as Map
import Data.Bool

--An array of categories in the format CategoryTable[category][data point]
    --The first column consists of unique category names
    --The first row consists of either "True" or "False" except for the first itme which is the category name
type CategoryTable = [[String]]

names :: CategoryTable -> [String]
names = map head

name :: Int -> CategoryTable -> String
name i ct = head $ ct !! i

numCategories :: CategoryTable -> Int
numCategories = length

numEntries :: CategoryTable -> Int
numEntries = length . tail . head

removeCategory :: String -> CategoryTable -> CategoryTable
removeCategory name = filter (\cat -> head cat /= name)

keepEntries :: [Int] -> CategoryTable -> CategoryTable
keepEntries keepIndices ct = removeEntries removeIndices ct
    where removeIndices = [0..(length ct)] \\ keepIndices

removeEntries :: [Int] -> CategoryTable -> CategoryTable
removeEntries indices = map (removeIndices indices)

--Removes the given indices from the list. Indices must be in sorted order.
removeIndices :: [Int] -> [a] -> [a]
removeIndices indices = removeIndicesRecursive indices 0

removeIndicesRecursive :: [Int] -> Int -> [a] -> [a]
removeIndicesRecursive _ _ [] = []
removeIndicesRecursive [] _ remainingList = remainingList
removeIndicesRecursive rList@(r:rs) o (x:xs)
    | r + o == 0 = removeIndicesRecursive (tail rList) (o-1) xs
    | otherwise = x : removeIndicesRecursive rList (o-1) xs

--Useless function can delete
removeAtIndex :: Int -> [a] -> [a]
removeAtIndex _ []     = []
removeAtIndex n (x:xs)
    | n == 0    = xs
    | otherwise = x : removeAtIndex (n-1) xs

successColumn :: CategoryTable -> [Bool]
successColumn  = map read . head

tableValue :: CategoryTable -> Maybe Bool
tableValue ct
    | numTrue == 0 = Just False
    | numTrue == numEntries ct = Just True
    | otherwise = Nothing
    where numTrue = length . filter id $ successColumn ct

--      ~Bin functions~
data Bin = Bin { entries :: [Int]
                ,response :: String
                , successCount :: Int
                , failCount :: Int
                } deriving (Show)

count :: Bin -> Int
count b = successCount b + failCount b

--Given a column index and category table, return a list of bins
bins :: Int -> CategoryTable -> [Bin]
bins catIndex ct = map snd (toList hashMap)
    where
        hashMap = foldl' insertToHashmap Map.empty kvs

        --Pairs of bin names and success bool
        kvs :: [(String, Bin)]
        kvs = zipWith3 pairKV (tail $ ct !! catIndex) (tail $ successColumn ct) [0..]

        pairKV :: String -> Bool -> Int -> (String, Bin)
        pairKV s b i = (s, Bin [i] s (bool 1 0 b) (bool 0 1 b))

insertToHashmap :: Map String Bin -> (String, Bin) -> Map String Bin
insertToHashmap m (k,v) = insertWith adder k v m
    where
        adder :: Bin -> Bin -> Bin
        adder new old = Bin (entries old ++ entries new) (response new) (successCount new + successCount old) (failCount new + failCount old)

--useless function, delete
binFromIndex :: Int -> CategoryTable -> [String]
binFromIndex index = nub . tail . (!! index)

--If all successes = Just True, if all failures = Just False, else Nothing
--Precondition: At least 1 success or failure
binValue :: Bin -> Maybe Bool
binValue (Bin _ _ 0 _) = Just True
binValue (Bin _ _ _ 0) = Just False
binValue _           = Nothing

--      ~CSV Loading~
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