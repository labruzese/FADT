import CSVLoader
import Data.Graph (Tree)
import Data.List (foldl', transpose)
import Data.Tree (unfoldTreeM_BF)
import Data.Foldable (maximumBy)
import Control.Monad.State
import Data.List.NonEmpty (nub)
import Data.Function (on)



examples = transpose

data Node = QuestionWithPrevAnswer (String, String) | Leaf Bool



decisionTree :: CategoryTable -> Tree Node
decisionTree ct = map fst $ unfoldTree findNextBestCategory (maximumBy importance ct, [])

--Current CT with a list of previous CTs and the answer it came from -> A list of the next levels Nodes and previous category tabke
findNextBestCategory :: (Node, [CategoryTable]) -> ([Node], [CategoryTable])
findNextBestCategory (node, (currCats : prevCats))
    | null $ tail $ successColumn currCats = plurityValue (head prevCats)
    | and $ tail $ successColumn currCats = ([Leaf $ successColumn currCats !! 1], currCats : prevCats)
    | null $ tail currCats = plurityValue currCats
    | otherwise = (uniqueWithIndices bestCategory)
    where 
        plurityValue category = ([Leaf $ mostFrequentItem $ successColumn category], currCats : prevCats)
        bestCategory = maximumBy (importance currCats) currCats

uniqueWithIndices :: Eq a => [a] -> [(a, [Int])]
uniqueWithIndices list = map (\x -> (x, elemIndices x list)) $ nub list


mostFrequentItem :: (Eq a) => [a] -> a
mostFrequentItem (x:xs) = fst $ maximumBy (compare `on` snd) $ map (\y -> (y, count y (x:xs))) (x:xs)
    where
        count x = length . filter (== x)


--takes in currCats and parentCat and returns the next level
-- learnTree :: CategoryTable -> CategoryTable -> [Classification]
-- learnTree currCats parentCats
--     | null (tail $ successColumn currCats) = [mostCommonOutput parentCats]
--     | all (== (successColumn currCats !! 1)) (drop 2 $ successColumn currCats)
--         = [Classification (successColumn currCats !! 1) ""]
--     | null $ tail currCats = [mostCommonOutput currCats]
--     | otherwise = continueBuilding
--     where
--         continueBuilding = maximumBy (importance currCats) 