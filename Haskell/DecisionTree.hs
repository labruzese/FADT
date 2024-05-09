module DecisionTree
(DecisionTree,
DTNodeValue,
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

type DecisionTree = Tree DTNode
--      ~Tree creation~
green :: String
green = "\ESC[32m"

red :: String
red = "\ESC[31m"

purple :: String
purple = "\ESC[35m"

clear :: String
clear = "\ESC[0m"

data DTNodeValue = Question String | Decision Bool | NoDecision deriving (Read)
instance Show DTNodeValue where
    show :: DTNodeValue -> String
    show (Question q) = purple ++ "¿" ++ q ++ "?" ++ clear
    show (Decision b) = if b
            then green ++ show b ++ clear  -- Green for True
            else red ++ show b ++ clear  -- Red for False
    show NoDecision = "No Decision"

type DTResponse = Maybe String

data DTNode = DTNode { dtResponse :: DTResponse
                , value :: DTNodeValue
                } deriving (Read)
instance Show DTNode where
    show (DTNode (Just r) v) = r ++ " -> " ++ show v
    show (DTNode Nothing v) = "root -> " ++ show v

decisionTree :: CategoryTable -> DecisionTree
decisionTree ct = decisionTreeNodeRecursive ct Nothing

decisionTreeNodeRecursive :: CategoryTable -> DTResponse -> DecisionTree
decisionTreeNodeRecursive ct response
    | numCategories ct == 0 = Node (DTNode response NoDecision) []
    | tValue == Just True = Node (DTNode response (Decision True)) []
    | tValue == Just False = Node (DTNode response (Decision False)) []
    | otherwise = Node (DTNode response (Question (question (category ct hcc)))) subNodes --Examples are mixed, bin and repeat
    where
        tValue = tableValue ct --trace("\ntvalue: " ++ show (tableValue ct) ++ "     Category table:" ++ show(ct) ++ "\n")
        hcc = highestCorrelatedCategoryWithoutGR ct
        bs = bins hcc ct --trace ("\nbins: " ++ show (bins hcc ct))
        subTables = map (\bin -> removeCategory (question (category ct hcc)) $ keepEntries (entries bin) ct) bs --bins -> subtables
        responses = map (Just . CSVLoader.response) bs
        subNodes = zipWith decisionTreeNodeRecursive subTables responses

--      ~Tree printing~
--Print all levels of a tree horizontally with each level on a separate line
printLevels :: Show a => Tree a -> IO ()
printLevels = putStrLn  . intercalate "" . intercalate ["\n"] . map (map show) . Data.Tree.levels

printTree :: Show a => Tree a -> IO ()
printTree = putStrLn . drawTree . fmap show

--      ~Decision making~
-- Just Bool represents the decision, Nothing represents a failure to make a decision
makeDecision :: DecisionTree -> Entry -> Maybe Bool
makeDecision tree entry = case currNodeValue of Decision b -> Just b
                                                Question s -> case nFromQ s of  Just d -> makeDecision d entry
                                                                                Nothing -> Nothing
                                                NoDecision -> Nothing
    where
        currNodeValue = value $ rootLabel tree
        nFromQ = nodeFromQuestion tree entry
        

--Given a question, pulls the response from the example and finds the correct child node
nodeFromQuestion :: DecisionTree -> Entry -> String -> Maybe DecisionTree
nodeFromQuestion tree entry = nodeFromResponse tree . responseFromExample entry

--Given an example and a question, finds the example's response
responseFromExample :: Entry -> String -> String
responseFromExample entry q = case index of Just i -> snd (answers entry !! i)
                                            Nothing -> error "Question not found in example"
    where index = findIndex (\c -> fst c == q) (answers entry) --Finds the index of the question(category name)

--Finds the child node with the given response
nodeFromResponse :: DecisionTree -> String -> Maybe DecisionTree
nodeFromResponse tree r = case index of Just i -> Just (children !! i)
                                        Nothing -> Nothing
    where
        match :: DecisionTree -> Bool
        match t = r == fromMaybe "" (dtResponse $ rootLabel t)

        children = subForest tree
        index = findIndex match children