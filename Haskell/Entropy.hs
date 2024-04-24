import CSVLoader
import GHC.Float (log1pDouble)
import GHC.Generics (D)
import Data.List ( minimumBy )
import Data.Function (on)


informationGain :: Double -> Int -> CategoryTable -> Double
informationGain tableEntropy category table = tableEntropy - entropyRemainingAfterTest category table

tableEntropy :: CategoryTable -> Double
tableEntropy ct = entropyOfBernoulliVariable $ fromIntegral trueCount / fromIntegral totalCount
    where
        successCol = successColumn ct
        trueCount = length $ filter id successCol
        totalCount = length successCol

highestCorrelatedCategory :: CategoryTable -> Int
highestCorrelatedCategory ct = fst (minimumBy (compare `on` snd) entropyPerCategory)
    where
        entropyPerCategory = zip [0..] $ map (`entropyRemainingAfterTest` ct) $ take (numCategories ct) [0..]

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
        nTotal = fromIntegral numEntries

--The entropy of a Boolean random variable that is true with probability p
entropyOfBernoulliVariable :: Double -> Double
entropyOfBernoulliVariable 0 = 0
entropyOfBernoulliVariable 1 = 0
entropyOfBernoulliVariable p = -(p * log2 p + q * log2 q)
    where
        q = 1-p
        log2 = logBase 2

