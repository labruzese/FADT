import System.IO
import Data.List (isInfixOf)

loadCategories :: FilePath -> IO [[String]]
loadCategories path = do
    content <- readFile path
    return $ formatCategories $ formatMissing $ invertTable $ map (splitOn ',') (lines content)



splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn delim xs = case break (== delim) xs of
    (before, []) -> [before]
    (before, _:ys) -> before : splitOn delim ys

invertTable :: [[a]] -> [[a]]
invertTable [] = []
invertTable ([]:_) = []
invertTable matrix = map head matrix : invertTable (map tail matrix)

--Header to transform to string
filterType :: String -> (String -> String)
filterType header
    | "Grade" `isInfixOf` header || "Q1" `isInfixOf` header = gradeFilter
    -- | "Teacher" `isInfixOf` header = teacherFilter
    -- | "Level" `isInfixOf` header = levelFilter
    -- | "World Language" `isInfixOf` header = wlFilter
    -- | "LP" `isInfixOf` header = lpFilter
    -- | "Birth Month" `isInfixOf` header = dobFilter
    -- | "Siblings" `isInfixOf` header = siblingFilter
    | otherwise = id

missingFilter :: String -> String
missingFilter str
    | str == "MISSING" = "N/A"
    | otherwise = str

formatMissing :: [[String]] -> [[String]]
formatMissing = map (map missingFilter . tail)

gradeFilter :: String -> String
gradeFilter item
    | strippedItem > 'C' = "C or worse"
    | otherwise = [strippedItem]
    where strippedItem = head $ item `removeSuffix` '+' `removeSuffix` '-'


removeSuffix :: String -> Char -> String
removeSuffix [] _ = []
removeSuffix str c
    | c == last str = init str
    | otherwise = str

formatCategories :: [[String]] -> [[String]]
formatCategories = map (\cat -> map (filterType (head cat)) (tail cat))