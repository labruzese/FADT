module DecisionTree where

import CSVLoader
import Data.List ( maximumBy, minimumBy, elemIndex, nub, transpose )
import Data.Function (on)
import Data.Ord (comparing)
import GHC.OldList (intercalate)

--MAIN CALLS--

m = do
    dt <- decisionTreeFromCSV
    putStrLn $ drawTree dt

decisionTreeFromCSV :: IO Node
decisionTreeFromCSV = CSVLoader.main >>= \cts -> return $ decisionTree (cts, [[]])

--DATA TYPES AND PRINTING--

data NodeData = Question String | Answer Bool

data Edge = Edge { label :: String, destination :: Node }
data Node = Node { nodeData :: NodeData, edges :: [Edge] }

instance Show NodeData where
    show (Answer x) = (if x then "\ESC[0;32m" else "\ESC[0;31m") ++ show x ++ "\ESC[0m"
    show (Question x) = "\ESC[0;35m" ++ show x ++ "\ESC[0m"
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

--DECISION TREE MAIN-

decisionTree :: (CategoryTable, CategoryTable) -> Node
decisionTree (currCT, prevCTS)
    | null $ successColumn currCT = plurityValue prevCTS
    | and (successColumn currCT) = Node (Answer True) []
    | all not (successColumn currCT) = Node (Answer False) []
    | null $ tail currCT = plurityValue currCT
    | otherwise = Node (Question $ head bestCategory) (map (uncurry edgeCreator) (subsets bestCategoryID currCT))
    where
        plurityValue category = Node (Answer $ mostFrequentItem $ successColumn category) []
        bestCategory = currCT !! bestCategoryID
        bestCategoryID = minimumBy (comparing $ remainingEntropy currCT) [1..length currCT - 1]
        edgeCreator label newCTS = Edge label $ curry decisionTree newCTS prevCTS

subsets :: Int -> CategoryTable -> [(String,CategoryTable)]
subsets catID ct = map (\i -> (i, subset ct i cat)) (featuresIn cat)
    where cat = ct !! catID

subset :: CategoryTable -> String -> Category -> CategoryTable
subset ct feature cat = transpose $ head (transpose ct) : map (transpose ct !!) acceptableIDs
    where
        acceptableIDs = filter (\i -> cat !! i == feature) [0..length cat - 1]

featuresIn :: Category -> [String]
featuresIn cat = nub $ tail cat

--ENTROPY--

remainingEntropy :: CategoryTable -> Int -> Double
remainingEntropy ct catID = sum $ map (individualEntropy . snd) (subsets catID ct)
    where
        individualEntropy ct2 = (numEnts ct2/numEnts ct) * entropyOfBool (posExs ct2 / numEnts ct2)
        posExs x = fromIntegral $ posExamples x
        numEnts x = fromIntegral $ numEntries x

entropyOfBool :: Double -> Double
entropyOfBool 0 = 0
entropyOfBool 1 = 0
entropyOfBool p = -(p * log2 p + q * log2 q)
    where
        q = 1-p
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