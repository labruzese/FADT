module Entropy
(highestCorrelatedCategory,
highestCorrelatedCategoryWithoutGR
) where

import CSVLoader
import GHC.Float (log1pDouble)
import GHC.Generics (D)
import Data.List ( minimumBy, maximumBy )
import Data.Function (on)
import Debug.Trace

--Given a category table, returns the index of the column [1..] that has the greatest decrease in entropy
highestCorrelatedCategory :: CategoryTable -> Int
highestCorrelatedCategory ct = fst (maximumBy (compare `on` snd) informationGainPerCategory)
    where
        tEntropy = tableEntropy ct

        informationGainPerCategory :: [(Int, Double)] --Int is the category index [1..], Double is the entropy remaining after the test
        informationGainPerCategory = zip [1..] $ map (\catIndex -> gainRatio tEntropy catIndex ct) $ take (numCategories ct) [1..]

highestCorrelatedCategoryWithoutGR :: CategoryTable -> Int
highestCorrelatedCategoryWithoutGR ct = fst (minimumBy (compare `on` snd) entropyPerCategory)
    where
        entropyPerCategory :: [(Int, Double)] --Int is the category index [1..], Double is the entropy remaining after the test
        entropyPerCategory = zip [1..] $ map (`entropyRemainingAfterTest` ct) $ take (numCategories ct) [1..]

gainRatio :: Double -> Int -> CategoryTable -> Double
gainRatio tableEntropy category table = informationGain tableEntropy category table / splitInfoAfterTest category table

informationGain :: Double -> Int -> CategoryTable -> Double
informationGain tableEntropy category table = tableEntropy - entropyRemainingAfterTest category table

tableEntropy :: CategoryTable -> Double
tableEntropy ct = entropyOfBernoulliVariable $ fromIntegral trueCount / fromIntegral totalCount
    where
        successCol = successColumn ct
        trueCount = length $ filter id successCol
        totalCount = length successCol

--The entropy after testing a category
entropyRemainingAfterTest :: Int -> CategoryTable -> Double
entropyRemainingAfterTest colNum ct = sum $ map (`binEntropy` n) bs
    where
        bs = bins colNum ct
        n = numEntries ct

binEntropy :: Bin -> Int -> Double
binEntropy bin numEntries = entropyOfBernoulliVariable (pCount/nBin) * nBin / nTotal
    where
        pCount = fromIntegral $ successCount bin
        nBin = fromIntegral $ count bin
        nTotal = fromIntegral numEntries --technically don't need this if just for comparison

splitInfoAfterTest :: Int -> CategoryTable -> Double
splitInfoAfterTest colNum ct = sum $ map (`splitInfo` n) bs
    where
        bs = bins colNum ct
        n = numEntries ct

splitInfo :: Bin -> Int -> Double
splitInfo bin numEntries = -(pBin * log2 pBin)
    where
        pCount = fromIntegral $ successCount bin
        nBin = fromIntegral $ count bin
        pBin = entropyOfBernoulliVariable (pCount/nBin)

--The entropy of a Boolean random variable that is true with probability p
entropyOfBernoulliVariable :: Double -> Double
entropyOfBernoulliVariable 0 = 0
entropyOfBernoulliVariable 1 = 0
entropyOfBernoulliVariable p = -(p * log2 p + q * log2 q)
    where
        q = 1-p

log2 = logBase 2