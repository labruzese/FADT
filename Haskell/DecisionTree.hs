module DecisionTree where

import CSVLoader
import Data.List ( maximumBy, elemIndex, nub, transpose )
import Data.Foldable (maximumBy)
import Data.Function (on)
import Data.Ord (comparing)

data NodeData = Question String | Answer Bool

data Edge = Edge { label :: String, destination :: Node }
data Node = Node { nodeData :: NodeData, edges :: [Edge] }

--DECISION TREE MAIN--

decisionTree :: [CategoryTable] -> Node
decisionTree (currCT : prevCTS)
    | null $ tail $ successColumn currCT = plurityValue (head prevCTS)
    | and $ tail $ successColumn currCT = Node (Answer $ successColumn currCT !! 1) []
    | null $ tail currCT = plurityValue currCT
    | otherwise = Node (Question $ head bestCategory) (zipWith edgeCreator (featuresIn bestCategory) (subsets bestCategory currCT))
    where
        plurityValue category = Node (Answer $ mostFrequentItem $ successColumn category) []
        bestCategory = maximumBy (comparing $ importance currCT) currCT
        bestCategoryID = findIndexInList bestCategory currCT
        edgeCreator label newCTS = Edge label $ decisionTree (newCTS : prevCTS)

subsets :: Category -> CategoryTable -> [CategoryTable]
subsets cat ct = map (\x -> subset ct x (findIndexInList cat ct)) (featuresIn cat)

subset :: CategoryTable -> String -> Int -> CategoryTable
subset ct feature categoryID = transpose $ filter (\example -> example !! categoryID /= feature) exampleTable
    where exampleTable = transpose ct

featuresIn :: Category -> [String]
featuresIn cat = nub $ tail cat

--ENTROPY--

importance :: CategoryTable -> Category -> Double
importance ct cat = entropyOfBool pGoal - remainingEntropy ct cat
    where 
        pGoal = p / (p+n)
        p = fromIntegral $ posExamples ct
        n = fromIntegral $ negExamples ct

remainingEntropy :: CategoryTable -> Category -> Double
remainingEntropy ct cat = sum $ map (\s -> proportionRemaining s * entropyOfBool (propSuccess s)) (subsets cat ct)
    where
        proportionRemaining sub = examples sub / examples ct
        propSuccess sub = p sub / examples ct
        p ctable = fromIntegral $ posExamples ctable
        examples ctable = fromIntegral $ numEntries ctable

entropyOfBool :: Double -> Double
entropyOfBool 0 = 0
entropyOfBool 1 = 1
entropyOfBool probability = -( sum $ map entropyOfVar [probability, 1-probability] )
    where
        entropyOfVar x = x * log2 x
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