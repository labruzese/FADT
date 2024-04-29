module TreeTesting
(
    main
) where

import CSVLoader
import DecisionTree
import Entropy

--Given an example and decision, checks if the decision matches the example decision
validateMaybeExample :: Example -> Maybe Bool -> Maybe Bool
validateMaybeExample ex Nothing = Nothing
validateMaybeExample ex (Just b) = Just $ validateExample ex b

validateExample :: Example -> Bool -> Bool
validateExample ex decision = decision == trueResult
    where trueResult = read $ snd (head ex)

testExample :: Example -> DecisionTree -> Maybe Bool
testExample ex dt = validateMaybeExample ex (makeDecision dt ex)

main :: IO ()
main = do
    --Load Category table
    ct <- loadCategoryTable "49dtd.csv"

    --Build decision tree
    let dt = decisionTree $ keepEntries [20..48] ct

    --Print stuff
    putStr . concat $ replicate 10 "\n"
    printTree dt

    --Pull examples
    let examples = pullExamples ct [0..19]
    let tests = map (\ex -> maybe "Nothing" show (testExample ex dt)) examples
    print tests
    let numCorrect = length (filter (=="True") tests)
    let totalTests = length tests
    putStrLn $ show numCorrect ++ "/" ++ show totalTests ++ " = " ++ show (fromIntegral numCorrect / fromIntegral totalTests)
