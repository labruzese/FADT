import CSVLoader
import Data.Tree
import Data.Time (CalendarDiffTime(ctMonths))
import Entropy

data DTNodeValue = Question String | Decision Bool deriving (Show)
type DTResponse = Maybe String
data DTNode = DTNode { response :: DTResponse
                , value :: DTNodeValue
                } deriving (Show)

decisionTreeEx :: Tree DTNode
decisionTreeEx = Node (Question "Cat1") [Node (Decision True) [], Node (Decision False) []]

decisionTree :: CategoryTable -> Tree DTNode
decisionTree ct
    |entropyRemainingAfterTest hcc ct == 0 = Node (Name (ct !! highestCorrelatedCategory))
    |otherwise = decisionTreeEx
    where 
        hcc = highestCorrelatedCategory ct

decisionTreeNodeRecursive :: CategoryTable -> DTResponse -> Node DTNode
decisionTreeNodeRecursive ct response
    | tValue == Just True = Node (DTNode response (DTValue (Decision True))) [] --All examples are true
    | tValue == Just False = Node (DTNode response (DTValue (Decision False))) [] --All examples are false
    | otherwise = Node (DTNode response (DTValue (Question name))) [] --Examples are mixed, bin and repeat
    where 
        tValue = tableValue ct
        hcc = highestCorrelatedCategory ct
        bs = bins hcc ct
        subTables = map (\bin -> keepEntries (entries bin) ct ) bs --
        subNodes = map (\n -> Node (DTNode )) subTables
        