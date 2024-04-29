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
CategoryTable(),
Example,
pullExample,
pullExamples
) where

import System.IO
import Data.List ( foldl', (\\), isInfixOf, nub, transpose )
import Data.Map.Strict (insertWith, Map, singleton, toList)
import qualified Data.Map.Strict as Map
import Data.Bool
import Debug.Trace
import Distribution.Compat.Prelude (readMaybe)
import Data.Maybe (isNothing)

--An array of categories in the format CategoryTable[category][Examples]
    --The first row consists of either "True" or "False" except for the first itme which is the category name
    --The first column consists of unique category names
type CategoryTable = [[String]]

names :: CategoryTable -> [String]
names = map head

--The 0th category is the success category
name :: Int -> CategoryTable -> String
name i ct = head $ ct !! i

--Does not count the success category
numCategories :: CategoryTable -> Int
numCategories ct = length ct - 1

--The "category names" row does not count as an entry
numEntries :: CategoryTable -> Int
numEntries = length . tail . head

removeCategory :: String -> CategoryTable -> CategoryTable
removeCategory name = filter (\cat -> head cat /= name)

--Entries start at 1 (the 0th entry is the category names row and is always kept)
keepEntries :: [Int] -> CategoryTable -> CategoryTable
keepEntries keepIndices ct = removeEntries removeIndices ct
    where removeIndices = [1..(1 + numEntries ct)] \\ keepIndices

--Entries start at 1 (the 0th entry is the category names row)
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

--Returns the success Column minus its category header
successColumn :: CategoryTable -> [Bool]
successColumn ct = map read (tail $ head ct)

--Looks at the entire table and returns 
--  Just False if everything is false
--  Just True if everything is true
--  Nothing if there is a mix
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
        kvs = zipWith3 pairKV (tail $ ct !! catIndex) (successColumn ct) [1..] -- (cat responses, success bool, entry index(starting at 1))

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
    return . formatCategories . formatMissing . transpose $ map (splitOn ',') (lines content)

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn delim xs = case break (== delim) xs of
    (before, []) -> [before]
    (before, _:ys) -> before : splitOn delim ys


--      ~Overall Filters~
missing = "N/A"

missingFilter :: String -> String
missingFilter "MISSING" = missing
missingFilter str = str

--Applies the missing filter
formatMissing :: CategoryTable -> CategoryTable
formatMissing = map (map missingFilter)


--      ~Specific Filters~
--Applies specific filters to each category
formatCategories :: CategoryTable -> CategoryTable
formatCategories = map (\cat ->
    let catName = head cat
    in catName : map (filterType catName) (drop 1 cat))

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

purgeExampleIf :: CategoryTable -> ([String] -> Bool) -> CategoryTable
purgeExampleIf ct f = transpose $ head flipped : filter f (tail flipped)
    where flipped = transpose ct
    
--      ~Testing examples~
type Example = [(String, String)] --[(Question, Response)]

--Examples start at index 1
pullExample :: CategoryTable -> Int -> Example
pullExample ct index = zip (head transposedTable) (exampleData transposedTable index)
    where transposedTable = transpose ct

pullExamples :: CategoryTable -> [Int] -> [Example]
pullExamples ct = map (zip (head transposedTable) . exampleData transposedTable)
    where transposedTable = transpose ct

exampleData :: CategoryTable -> Int -> [String]
exampleData transposedTable index = transposedTable !! index
