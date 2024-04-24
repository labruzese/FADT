module DecisionTree
(DTNodeValue,
DTResponse,
DTNode,
decisionTree,
printLevels,
printTree
) where

import CSVLoader
import Data.Tree
import Entropy
import Debug.Trace
import Data.List (intersperse, intercalate, findIndex)
import Data.Maybe (fromMaybe)

--      ~Tree creation~
data DTNodeValue = Question String | Decision Bool | NoDecision deriving (Read)
instance Show DTNodeValue where
    show :: DTNodeValue -> String
    show (Question q) = "¿" ++ q ++ "?"
    show (Decision b) = show b
    show NoDecision = "No Decision"

type DTResponse = Maybe String

data DTNode = DTNode { dtResponse :: DTResponse
                , value :: DTNodeValue
                } deriving (Read)
instance Show DTNode where
    show (DTNode (Just r) v) = "|" ++ r ++ " -> " ++ show v ++ "|  "
    show (DTNode Nothing v)  = "|root -> " ++ show v ++ "|  "

decisionTree :: CategoryTable -> Tree DTNode
decisionTree ct = decisionTreeNodeRecursive ct Nothing

decisionTreeNodeRecursive :: CategoryTable -> DTResponse -> Tree DTNode
decisionTreeNodeRecursive ct response
    | numCategories ct == 0 = Node (DTNode response NoDecision) []
    | tValue == Just True = Node (DTNode response (Decision True)) []
    | tValue == Just False = Node (DTNode response (Decision False)) []
    | otherwise = Node (DTNode response (Question (name hcc ct))) subNodes --Examples are mixed, bin and repeat
    where
        tValue = tableValue ct --trace("\ntvalue: " ++ show (tableValue ct) ++ "     Category table:" ++ show(ct) ++ "\n")
        hcc = highestCorrelatedCategory ct
        bs = bins hcc ct --trace ("\nbins: " ++ show (bins hcc ct))
        subTables = map (\bin -> removeCategory (name hcc ct) $ keepEntries (entries bin) ct) bs --bins -> subtables
        responses = map (Just . CSVLoader.response) bs
        subNodes = zipWith decisionTreeNodeRecursive subTables responses

--      ~Tree printing~
--Print all levels of a tree horizontally with each level on a separate line
printLevels :: Show a => Tree a -> IO ()
printLevels = putStrLn  . intercalate "" . intercalate ["\n"] . map (map show) . Data.Tree.levels

printTree :: Show a => Tree a -> IO ()
printTree = putStrLn . drawTree . fmap show

--      ~Decision making~
makeDecision :: Tree DTNode -> Example -> Maybe Bool
makeDecision tree example = case v of   Decision b -> Just b
                                        Question s -> case nodeFromQuestion s of    Just d -> makeDecision d example
                                                                                    Nothing -> Nothing
    where
        v = value $ rootLabel tree
        nodeFromQuestion :: String -> Maybe (Tree DTNode)
        nodeFromQuestion = nodeFromResponse tree . responseFromExample example


responseFromExample :: Example -> String -> String
responseFromExample e q = case index of Just i -> snd (e !! i)
                                        Nothing -> error "Question not found in example"
    where index = findIndex (\c -> fst c == q) e

nodeFromResponse :: Tree DTNode -> String -> Maybe (Tree DTNode)
nodeFromResponse tree r = case index of Just i -> Just (children !! i)
                                        Nothing -> Nothing
    where
        match :: Tree DTNode -> Bool
        match t = r == fromMaybe "" (dtResponse $ rootLabel t)

        children = subForest tree
        index = findIndex match children



main :: IO ()
main = do
        ct <- loadCategoryTable "49dtd.csv"
        let dt = decisionTree $ keepEntries [10..25] ct
        putStr . concat $ replicate 10 "\n"
        printTree dt
        let examples = pullExamples ct [31..48]
        let tests = map (\ex -> maybe "Nothing" show (testMaybeExample ex (makeDecision dt ex))) examples
        print tests
        let numCorrect = length (filter (=="True") tests)
        let totalTests = length tests
        putStrLn $ show numCorrect ++ "/" ++ show totalTests ++ " = " ++ show (fromIntegral numCorrect / fromIntegral totalTests)
