module DecisionTree where

import CSVLoader
import Data.List ( maximumBy, elemIndex, nub, transpose )
import Data.Foldable (maximumBy)
import Data.Function (on)
import Data.Ord (comparing)
import GHC.OldList (intercalate)
import Data.Tree (Tree)

data NodeData = Question String | Answer Bool deriving (Show)

data Edge = Edge { label :: String, destination :: Node }
data Node = Node { nodeData :: NodeData, edges :: [Edge] }
instance Show Node where
    show x = show (nodeData x) ++ "\n Answers: " ++ intercalate " or " (map show (edges x))

instance Show Edge where
    show x = show (label x)

drawTree :: Node -> String
drawTree  = unlines . draw

draw :: Node -> [String]
draw (Node n e) = lines (show n) ++ drawSubTrees e
  where
    drawSubTrees [] = []
    drawSubTrees [e] =
        ( "|" ++ show e ) : shift "`- " "   " (draw (destination e))
    drawSubTrees (e:es) =
        ( "|" ++ show e ) : shift "+- " "|  " (draw (destination e)) ++ drawSubTrees es

    shift first other = zipWith (++) (first : repeat other)

--DECISION TREE MAIN--

m = do
    dt <- decisionTreeFromCSV
    putStrLn $ drawTree dt

decisionTreeFromCSV :: IO Node
decisionTreeFromCSV = CSVLoader.main >>= \cts -> return $ decisionTree (cts, [[]])

decisionTree :: (CategoryTable, CategoryTable) -> Node
decisionTree (currCT, prevCTS)
    | null $ successColumn currCT = plurityValue prevCTS
    | and $ successColumn currCT = Node (Answer $ successColumn currCT !! 1) []
    | null $ tail currCT = plurityValue currCT
    | otherwise = Node (Question $ head bestCategory) (zipWith edgeCreator (featuresIn bestCategory) (subsets bestCategory currCT))
    where
        plurityValue category = Node (Answer $ mostFrequentItem $ successColumn category) []
        bestCategory = maximumBy (comparing $ importance currCT) currCT
        bestCategoryID = findIndexInList bestCategory currCT
        edgeCreator label newCTS = Edge label $ decisionTree (newCTS, prevCTS)

subsets :: Category -> CategoryTable -> [CategoryTable]
subsets cat ct = map (\x -> subset ct x (findIndexInList cat ct)) (featuresIn cat)

subset :: CategoryTable -> String -> Int -> CategoryTable
subset ct feature categoryID = transpose $ filter (\example -> example !! categoryID /= feature) exampleTable
    where exampleTable = transpose ct

featuresIn :: Category -> [String]
featuresIn cat = nub $ tail cat

--ENTROPY--

importance :: CategoryTable -> Category -> Double
importance ct cat = -remainingEntropy ct cat

remainingEntropy :: CategoryTable -> Category -> Double
remainingEntropy ct cat = sum $ map individualEntropy (subsets cat ct)
    where
        individualEntropy category = proportionRemaining category * entropyOfBool (propSuccess category)

        proportionRemaining subCT = examples subCT / examples ct
        propSuccess subCT = p subCT / examples subCT

        p x = fromIntegral $ posExamples x
        examples x = fromIntegral $ numEntries x

entropyOfBool :: Double -> Double
entropyOfBool 0 = 0
entropyOfBool 1 = 1
entropyOfBool p = -( p * log2 p + q * log2 q ) 
    where
        q = 1 - p
        log2 = logBase 2


--HELPERS--

findIndexInList :: Eq a => a -> [a] -> Int
findIndexInList x xs =
  case elemIndex x xs of
    Just idx -> idx
    Nothing  -> -1

mostFrequentItem :: (Eq a) => [a] -> a
mostFrequentItem (x:xs) = fst $ maximumBy (compare `on` snd) $ map (\y -> (y, count y (x:xs))) (x:xs)
    where
        count x = length . filter (== x)