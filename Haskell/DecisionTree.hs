module DecisionTree
(DTNodeValue,
DTResponse,
DTNode,
decisionTree,
printLevels,
printTree,
makeDecision
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
makeDecision tree example = case currNodeValue of   Decision b -> Just b
                                                    Question s -> case nFromQ s of  Just d -> makeDecision d example
                                                                                    Nothing -> Nothing
    where
        currNodeValue = value $ rootLabel tree
        nFromQ = nodeFromQuestion tree example
        

--Given a question, pulls the response from the example and finds the correct child node
nodeFromQuestion :: Tree DTNode -> Example -> String -> Maybe (Tree DTNode)
nodeFromQuestion tree example = nodeFromResponse tree . responseFromExample example

--Given an example and a question, finds the example's response
responseFromExample :: Example -> String -> String
responseFromExample e q = case index of Just i -> snd (e !! i)
                                        Nothing -> error "Question not found in example"
    where index = findIndex (\c -> fst c == q) e --Finds the index of the question(category name)

--Finds the child node with the given response
nodeFromResponse :: Tree DTNode -> String -> Maybe (Tree DTNode)
nodeFromResponse tree r = case index of Just i -> Just (children !! i)
                                        Nothing -> Nothing
    where
        match :: Tree DTNode -> Bool
        match t = r == fromMaybe "" (dtResponse $ rootLabel t)

        children = subForest tree
        index = findIndex match children