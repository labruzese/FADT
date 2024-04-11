import CSVLoader
import GHC.Float (log1pDouble)
import GHC.Generics (D)

--Reduction in entropy due to testing a category(might not be needed)
--informationGain :: Double -> Int -> CategoryTable -> Double
--informationGain tableEntropy category table = tableEntropy - entropyRemainingAfterTest category table

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

occurences :: String -> [String] -> Int
occurences match = length . filter (== match)

--bestEntropy :: CategoryTable -> [String]
--bestEntropy catTable = max $ mapWithIndex (\i -> entropyRemainingAfterTest i catTable) catTable

totalEntropy :: CategoryTable -> Double
totalEntropy ct = entropyOfBernoulliVariable $ fromIntegral trueCount / fromIntegral totalCount
    where
        successCol = successColumn ct
        trueCount = length $ filter id successCol
        totalCount = length successCol

--The entropy of a Boolean random variable that is true with probability p
entropyOfBernoulliVariable :: Double -> Double
entropyOfBernoulliVariable 0 = 0
entropyOfBernoulliVariable 1 = 0
entropyOfBernoulliVariable p = -(p * log2 p + q * log2 q)
    where
        q = 1-p
        log2 = logBase 2

