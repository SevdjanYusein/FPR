import Data.List

--Задача 1:
type Day = Int
type Degree = Float

data Measuring = Temp Day Degree
    deriving (Read, Show)

closest :: Float -> [Float] -> [Float]
closest avg [] = []
closest avg (x:[]) = []
closest avg (x:y:[]) = if abs (avg - x) > abs (avg - y) then [y] else [x]
closest avg (x:y:xs) = if abs (avg - x) > abs (avg - y)
                       then y : closest avg xs
                       else x : closest avg xs

closestToAverage :: [Measuring] -> [Int]    -- Съставя списък от дните с най-близката до средната за месеца температура
closestToAverage measurings = [day | (Temp day deg) <- measurings, deg `elem` closest average allDegrees]
                                     where allDegrees = [deg | (Temp day deg) <- measurings]
                                           average = sum allDegrees / fromIntegral (length allDegrees)


--Задача 2:
data BTree = Empty | Node Int BTree BTree
     deriving (Read, Show) 

treeNodesAtLevel :: BTree -> Int -> [Int]
treeNodesAtLevel Empty _ = []
treeNodesAtLevel (Node x _ _) 0 = [x]
treeNodesAtLevel (Node _ left right) n = treeNodesAtLevel left (n - 1) ++ treeNodesAtLevel right (n - 1)

allGrandBool :: BTree -> [Bool]
allGrandBool Empty = []
allGrandBool tree@(Node x left right) = [ x <= (grand + 1) | grand <- grandchildren] ++ xs
    where
    grandchildren = treeNodesAtLevel tree 2
    xs = allGrandBool left ++ allGrandBool right

treeDepth :: BTree -> Int
treeDepth Empty = 0
treeDepth (Node _ left right) = 1 + max (treeDepth left) (treeDepth right)

grandchildrenIncreased :: BTree -> Bool 
grandchildrenIncreased Empty = False
grandchildrenIncreased tree = if (treeDepth tree <= 2) then False else and (allGrandBool tree)


main :: IO ()   
main = do
 print $ closestToAverage [(Temp 1 23.6),(Temp 6 24.2),
                  (Temp 11 24.2),(Temp 16 21.2),
                  (Temp 21 23.8),(Temp 26 26.5),
                  (Temp 31 24.5)] -- → 6 или 11 или 21(средната температура е 24.0)
                  
 print$ grandchildrenIncreased (Node 3 (Node 1 (Node 10 Empty Empty) (Node 6 Empty Empty)) 
                               (Node 4 Empty (Node 5 Empty (Node 7 Empty Empty))))
