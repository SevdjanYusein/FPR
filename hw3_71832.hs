import Data.Char
import Data.List
import Data.Typeable

--Задача 1:
pairCompose :: [(Int -> Int)] -> (Int -> Int)
pairCompose [] = id
pairCompose (f:[]) = f.id
pairCompose (f:fs) = (\x -> ((f . (head fs)) x) + (pairCompose (tail fs) x))

--Задача 2:
switchsum :: (Int -> Int) -> (Int -> Int) -> Int -> (Int -> Int)
switchsum f g 1 = f
switchsum f g n = (\x -> if n`mod`2 == 0 
                         then (g x) + switchsum f g (n-1) (g x) 
                         else (f x) + switchsum f g (n-1) (f x))

--Задача 3:
replaceAssoc :: [Int] -> [(Int, Int)] -> [Int]
replaceAssoc [] dict = []
replaceAssoc list dict = replace (head list) dict : replaceAssoc (tail list) dict

replace x [] = x
replace x (y:ys)
      | x == fst y = snd y
      | otherwise = replace x ys
      
--Задача 4:

--findAssoc' :: [(Int, [Int])] -> Int -> Int
--findAssoc' xs key = head [k | (k, ys) <- xs, (sum ys) == key]
         -- В този случай ако няма такъв елемнт няма стойност като резултат

findAssoc :: [(Int, [Int])] -> Int -> Int
findAssoc [] key = -1
findAssoc (x:xs) key = if (sum $ snd x) == key 
                   then fst x
                   else findAssoc xs key
     {- За да се състави списък от елементите, отговарящи на условието подаваме невалидна стойност 
        на елементите, които не отговарят на изискванията (в случая -1) 
     -}

nodes :: [(Int, [Int])] -> [Int] -> [Int]
nodes tree [] = []
nodes tree (x:xs) = if findAssoc tree x > -1         -- С това условие съсзаваме списък с валидните елементи
                    then findAssoc tree x  : nodes tree xs 
                    else nodes tree xs

numOfNodes :: [(Int, [Int])] -> Int
numOfNodes tree = length $ nodes tree parents        -- Броим елементите в списъка
          where parents = map (\(p, _) -> p) tree


main :: IO ()
main = do
    print $ (pairCompose [(\x -> x+1),(\x -> x+2),(\x -> x+3)]) 1 -- → 8
   
    print $ switchsum (\x -> x + 1) (\x -> x * 2) 1 $ 2 -- → 3
    print $ switchsum (\x -> x + 1) (\x -> x * 2) 2 $ 2 -- → 9
    print $ switchsum (\x -> x + 1) (\x -> x * 2) 3 $ 2 -- → 16
    print $ switchsum (\x -> x + 1) (\x -> x * 2) 4 $ 2 -- → 30  
   
    print $ replaceAssoc [5,4,2,3] [(1,5),(3,7),(5,9),(7,11),(9,13)] -- →[9,4,2,7]
    
   -- print $ findAssoc [(10,[3,7,12]),(3,[5,8,9]),(7,[11,13]),(12,[6,4]),(8,[1,2])] 10
   -- print $ nodes [(10,[3,7,12]),(3,[5,8,9]),(7,[11,13]),(12,[6,4]),(8,[1,2])] [10,3,7,12,8]
    print $ numOfNodes [(10,[3,7,12]),(3,[5,8,9]),(7,[11,13]),(12,[6,4]),(8,[1,2])] -- → 2 //(върховете 12 и 8)