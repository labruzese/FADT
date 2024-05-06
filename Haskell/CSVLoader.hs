module CSVLoader
(CategoryTable,
results,
categories,
mapResults,
mapCategories,
category,
Category,
continuous,
question,
responses,
mapResponses,
Entry,
result,
answers,
mapAnswers,
numCategories,
numEntries,
removeCategory,
keepEntries,
removeEntries,
tableValue,
pullEntries,
Bin,
count,
entries,
response,
successCount,
failCount,
bins,
loadCategoryTable,
) where

import System.IO
import Data.List ( foldl', (\\), isInfixOf, nub, transpose )
import Data.Map.Strict (insertWith, Map, singleton, toList)
import qualified Data.Map.Strict as Map
import Data.Bool
import Debug.Trace
import Distribution.Compat.Prelude (readMaybe)
import Data.Maybe (isNothing)
import Language.Haskell.TH.PprLib (cat)

--      ~Structure~
--An array of categories
    --The first category's responses consists of either "True" or "False"
data CategoryTable = CategoryTable { results :: [Bool],
                                    categories :: [Category]
                                    } deriving(Show)


-- Map over results
mapResults :: (Bool -> a) -> CategoryTable -> [a]
mapResults f (CategoryTable rs _) = map f rs

-- Map over predictors
mapCategories :: (Category -> Category) -> CategoryTable -> CategoryTable
mapCategories f ct = ct{categories = map f (categories ct)}

--Pulls a predictor(category) 
category :: CategoryTable -> Int -> Category
category ct i = categories ct !! i

data Category = Category { continuous :: Bool,
                question :: String,
                responses :: [String]
                } deriving(Show)

mapResponses :: (String -> String) -> Category -> Category
mapResponses f cat = cat{responses = map f (responses cat)}

data Entry = Entry { result :: Bool,
                    answers :: [(String, String)]
                } deriving(Show)

mapAnswers :: ((String, String) -> (a, a)) -> Entry -> [(a, a)]
mapAnswers f e = map f (answers e)

--      ~Basic methods~
--Does not count the successCategory
numCategories :: CategoryTable -> Int
numCategories = length . categories

--The "category names" row does not count as an entry
numEntries :: CategoryTable -> Int
numEntries = length . results

removeCategory :: String -> CategoryTable -> CategoryTable
removeCategory q ct = ct { categories = filter (\predictor -> question predictor /= q) (categories ct)}

--Entries start at 0
keepEntries :: [Int] -> CategoryTable -> CategoryTable
keepEntries keepIndices ct = removeEntries removeIndices ct
    where removeIndices = [0..(numEntries ct)] \\ keepIndices

--Entries start at 0
removeEntries :: [Int] -> CategoryTable -> CategoryTable
removeEntries indices ct = CategoryTable (removeIndices indices (results ct)) (map (\c -> c{responses = removeIndices indices (responses c)}) (categories ct))

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

--Looks at the entire table and returns 
--  Just False if everything is false
--  Just True if everything is true
--  Nothing if there is a mix
tableValue :: CategoryTable -> Maybe Bool
tableValue ct
    | numTrue == 0 = Just False
    | numTrue == numEntries ct = Just True
    | otherwise = Nothing
    where numTrue = length . filter id $ results ct


--map out responses, transpose, map into Entries
pullEntries :: CategoryTable -> [Entry]
pullEntries ct = zipWith (\result r -> Entry result (zip questions r)) (results ct) entryResponses
    where 
        entryResponses = transpose $ map responses (categories ct)
        questions = map question (categories ct)
        

--      ~Bin functions~
data Bin = Bin { entries :: [Int]
                ,response :: String
                , successCount :: Int
                , failCount :: Int
                } deriving (Show)

count :: Bin -> Int
count b = successCount b + failCount b

--Given a category index(starting at 0) and category table, return a list of bins
bins :: Int -> CategoryTable -> [Bin]
bins catIndex ct = map snd (toList hashMap)
    where
        hashMap = foldl' insertToHashmap Map.empty kvs

        --Pairs of response(map key) and bins
        kvs :: [(String, Bin)]
        kvs = zipWith3 pairKV (responses (category ct catIndex)) (results ct) [0..] -- (cat responses, success bool, entry index(starting at 0))

        pairKV :: String -> Bool -> Int -> (String, Bin)
        pairKV s b i = (s, Bin [i] s (bool 1 0 b) (bool 0 1 b))

insertToHashmap :: Map String Bin -> (String, Bin) -> Map String Bin
insertToHashmap m (k,v) = insertWith adder k v m
    where
        adder :: Bin -> Bin -> Bin
        adder new old = Bin (entries old ++ entries new) (response new) (successCount new + successCount old) (failCount new + failCount old)

--useless function, delete
binFromIndex :: Int -> CategoryTable -> [String]
binFromIndex index ct = nub . responses $ category ct index

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
    let splitTable = transpose $ map (splitOn ',') (lines content) 
    let results = map (\g -> gradeComparator g < 1.67) (drop 1 $ head splitTable)

    return . formatCategories . formatMissing $ CategoryTable results (map readCategory (tail splitTable))

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn delim xs = case break (== delim) xs of
    (before, []) -> [before]
    (before, _:ys) -> before : splitOn delim ys

readCategory :: [String] -> Category
readCategory (q:rs) = Category (continuousFromString q) q rs

continuousFromString :: String -> Bool
continuousFromString q = any (`isInfixOf` q) ["Grade", "Abs+Tardies", "Birth Month", "Siblings", "Level"]
--      ~Binary expansion~

--      ~Continous data comparators~
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
            | otherwise = error "Illegal grade"

birthMonthComparator :: String -> Double
birthMonthComparator string =
    let month = read string :: Int
    in fromIntegral ((month + 3) `mod` 12)

classLevelComparator :: String -> Double
classLevelComparator "Honors" = 1
classLevelComparator "Advanced" = 2
classLevelComparator "Accelerated" = 3
classLevelComparator "CP" = 4

abstComparator :: String -> Double
abstComparator = read

siblingComparator :: String -> Double
siblingComparator = read

--      ~Overall Filters~
missing = "N/A"

missingFilter :: String -> String
missingFilter "MISSING" = missing
missingFilter str = str

--Applies the missing filter
formatMissing :: CategoryTable -> CategoryTable
formatMissing = mapCategories (mapResponses missingFilter)

--      ~Specific Filters~
--Applies specific filters to each category
formatCategories :: CategoryTable -> CategoryTable
formatCategories = mapCategories (\cat -> mapResponses (filterType (question cat)) cat)

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

-- purgeExampleIf :: CategoryTable -> ([String] -> Bool) -> CategoryTable
-- purgeExampleIf ct f = transpose $ head flipped : filter f (tail flipped)
--     where flipped = transpose ct

--      ~Testing examples~
-- type Example = [(String, String)] --[(Question, Response)]

--Examples start at index 1
-- pullExample :: CategoryTable -> Int -> Example
-- pullExample ct index = zip (head transposedTable) (exampleData transposedTable index)
--     where transposedTable = transpose ct

-- pullExamples :: CategoryTable -> [Int] -> [Example]
-- pullExamples ct = map (zip (head transposedTable) . exampleData transposedTable)
--     where transposedTable = transpose ct

-- exampleData :: CategoryTable -> Int -> [String]
-- exampleData transposedTable index = transposedTable !! index
