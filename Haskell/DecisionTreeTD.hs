import CSVLoader
import Data.Graph (Tree)
import Data.List
import Data.Tree (unfoldTreeM_BF)
import Data.Foldable (maximumBy)
import Control.Monad.State
import Data.Function (on)
import Data.Maybe (fromMaybe)
import GHC.Float (log1pDouble)
import Entropy



data NodeData = Question String | Answer Bool

data Edge = Edge { label :: String, destination :: Node }
data Node = Node { nodeData :: NodeData, edges :: [Edge] }

--Current CT with a list of previous CTs and the answer it came from -> A list of the next levels Nodes and previous category take

decisionTree :: [CategoryTable] -> Node
decisionTree (currCT : prevCTS)
    | null $ tail $ successColumn currCT = plurityValue (head prevCTS)
    | and $ tail $ successColumn currCT = Node (Answer $ successColumn currCT !! 1) []
    | null $ tail currCT = plurityValue currCT
    | otherwise = Node (Question $ head bestCategory) (zipWith edgeCreator (featuresIn bestCategory) (subsets bestCategory currCT))
    where
        plurityValue category = Node (Answer $ mostFrequentItem $ successColumn category) []
        bestCategory = maximumBy (importance currCT) currCT
        bestCategoryID = findIndexInList bestCategory currCT
        edgeCreator label newCTS = Edge label $ decisionTree (newCTS : prevCTS)


subsets :: Category -> CategoryTable -> [CategoryTable]
subsets cat ct = map (\x -> subset ct x (findIndexInList cat ct)) (featuresIn cat)

importance :: CategoryTable -> Category -> Double
importance ct cat = entropyOfBool pGoal - remainingEntropy cat
    where pGoal = posExamples ct / (posExamples ct + negExamples ct)

subset :: CategoryTable -> String -> Int -> CategoryTable
subset ct feature categoryID = transpose $ filter (\example -> example !! categoryID /= feature) exampleTable
    where exampleTable = transpose ct

featuresIn :: Category -> [String]
featuresIn cat = nub $ tail cat

posExamples :: CategoryTable -> Int
posExamples ct = count True $ successColumn ct
    where
        count x = length . filter (== x)

negExamples :: CategoryTable -> Int
negExamples ct = count False $ successColumn ct
    where
        count x = length . filter (== x)

findIndexInList :: Eq a => a -> [a] -> Int
findIndexInList x xs =
  case elemIndex x xs of
    Just idx -> idx
    Nothing  -> -1

mostFrequentItem :: (Eq a) => [a] -> a
mostFrequentItem (x:xs) = fst $ maximumBy (compare `on` snd) $ map (\y -> (y, count y (x:xs))) (x:xs)
    where
        count x = length . filter (== x)

entropyOfBool :: Double -> Double
entropyOfBool 0 = 0
entropyOfBool 1 = 1
entropyOfBool probability = -sum $ map entropyOfVar [probability, 1-probability]
    where 
        entropyOfVar x = x * log2 x
        log2 = logBase 2

remainingEntropy :: CategoryTable -> Category -> Double
remainingEntropy ct cat = sum $ map (\subset -> proportionRemaining subset ct * entropyOfBool $ propPos subset) subsets
    where 
        proportionRemaining subset = numEntries subset / numEntries ct
        propPos ct = posExamples / numEntries ct