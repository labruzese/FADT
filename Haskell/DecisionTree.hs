module DecisionTree() where

import CSVLoader
import Data.List ( maximumBy, minimumBy, elemIndex, nub, transpose, find )
import Data.Function (on)
import Data.Ord (comparing)
import GHC.OldList (intercalate)
import Debug.Trace (trace)
import Control.Monad ( (>=>) )

--MAIN CALLS--

m = do
    dt <- decisionTreeFromCSV
    putStrLn $ drawTree dt

decisionTreeFromCSV :: IO Node
decisionTreeFromCSV = CSVLoader.main >>= \cts -> return $ decisionTree (cts, Nothing)

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

--DECISION TREE MAIN--
dt :: CategoryTable -> Node
dt x = decisionTree (x,Nothing)

decisionTree :: (CategoryTable, Maybe CategoryTable) -> Node
decisionTree (currCT, prevCT)
    | null $ dataPoints $ results currCT = plurityValue $ tryGet prevCT
    | and $ dataPoints $ results currCT = Node (Answer True) []
    | all not $ dataPoints $ results currCT = Node (Answer False) []
    | null $ predictors currCT = plurityValue currCT
    | otherwise = Node (Question $ question bestCategory ++ "? " ++ divisionLabel bestCategory) $ edgeCreator (subsets currCT bestCategory)
    where
        plurityValue ct = Node (Answer $ mostFrequentItem $ dataPoints $ results ct) []
        bestCategory = maximumBy (comparing $ informationGain currCT) (predictors currCT)
        edgeCreator subs = [
            Edge "Yes" $ curry decisionTree (yesPath subs) $ Just currCT,
            Edge "No" $ curry decisionTree (noPath subs) $ Just currCT
            ]
        tryGet prevCT = case prevCT of
            Just prevCT -> prevCT
            Nothing -> error "Not enough data to train tree"

data Subsets = Subsets {yesPath :: CategoryTable, noPath :: CategoryTable} deriving (Show)
toList :: Subsets -> [CategoryTable]
toList subsets = [yesPath subsets, noPath subsets]

subsets :: CategoryTable -> Category -> Subsets
subsets ct cat = Subsets (pruneEntriesFromCT acceptableIDs ct) (pruneEntriesFromCT nonAcceptableIds ct)
    where
        acceptableIDs = filter (\i -> dataPoints cat !! i) [0..length (dataPoints cat) - 1]
        nonAcceptableIds = filter (\i -> not $ dataPoints cat !! i) [0..length (dataPoints cat) - 1]

--ENTROPY--
informationGain :: CategoryTable -> Category -> Double
informationGain ct cat = entropy (posRatio ct) - remainingEntropy ct cat
    where 
        posRatio x = fromIntegral (posExamples x) / numEnts x
        numEnts x = fromIntegral $ numEntries x

remainingEntropy :: CategoryTable -> Category -> Double
remainingEntropy ct cat = 
    let subs = subsets ct cat in 
    if numEnts (yesPath subs) == 0 || numEnts (noPath subs) == 0 
        then entropy (posRatio ct) 
    else 
        sum $ map individualEntropy $ toList subs
    where
        individualEntropy ct2 = sizeRatio ct2 * entropy (posRatio ct2)
        sizeRatio x = numEnts x / numEnts ct
        posRatio x = fromIntegral (posExamples x) / numEnts x
        numEnts x = fromIntegral $ numEntries x

entropy :: Double -> Double
entropy 0 = 0
entropy 1 = 0
entropy p = -(p * log2 p + q * log2 q)
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

-- TESTING --
-- test :: Node -> NewCategoryTable -> (Int,Int)
-- test dt ct = (count True (zipWith (==) realAnswers answers), numEntries ct)
--     where
--         answers = map (interrogate dt . zip (headers ct) . (entries ct !!)) [0..numEntries ct - 1]
--         count x = length . filter (==x)
--         realAnswers = successColumn ct

interrogate :: Node -> [(String, String)] -> Bool
interrogate (Node (Answer a) _) _ = a
interrogate (Node (Question q) edges) example = interrogate (answerNodeFrom q) example
    where
        answerNodeFrom q = case find ((==answer q) . Just . label) edges of
            Just x -> destination x
            Nothing -> Node (Answer False) []
        answer :: String -> Maybe String
        answer question = Just . snd =<< find ((== question) . fst) example

-- run :: Int -> IO TestResult
-- run sp = do
--     size <- fromIntegral . numEntries <$> loadTrainingSet
--     trainCSV <- trim sp <$> loadTrainingSet
--     testCSV <- trim2 sp <$> loadTrainingSet
--     let d = dt trainCSV
--     return $ uncurry TestResult $ test d testCSV

data TestResult = TestResult { successes :: Int, total :: Int }
instance Show TestResult where
    show :: TestResult -> String
    show t = show p ++ "/" ++ show n ++ " -> " ++ show (fromIntegral p / fromIntegral n)
        where
            p = successes t
            n = total t

-- a :: IO ()
-- a = mapM_ (\p -> run p >>= \result -> putStrLn (show p ++ " | " ++ show result)) [1..14]

-- b sp = do
--     size <- fromIntegral . numEntries <$> main
--     trainCSV <- trim sp <$> loadTrainingSet
--     testCSV <- trim2 sp <$> loadTrainingSet
--     putStrLn $ drawTree $ dt trainCSV