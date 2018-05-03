import Data.List
import Data.Char

{-
  Зад. 1. Хетерограми: дефинирайте функцията isHeterogram str, която проверява дали
  символният низ str e хетерограма. Хетерограма се нарича символен низ, в който всеки
  символ се среща само по веднъж.

  Примери:
    isHeterogram "abcd" = True
    isHeterogram "abbd" = False
-}
isHeterogram :: String -> Bool
isHeterogram str = length str == length (nub str)

{-
  Зад. 2.
    a). Дефинирайте функцията vectSum xs ys, която връща сбора на векторите xs и ys.

    Примери:
      vectSum [1, 2, 3] [4, 5, 6] = [5, 7, 9]

    б). Дефинирайте функцията scalarProd xs ys, която връща скаларното произведение на
    векторите xs и ys.

    Примери:
      scalarProd [1, 2, 3] [4, 5, 6] = 32
-}
vectSum :: Num a => [a] -> [a] -> [a]
vectSum xs ys = map (\(x, y) -> x + y ) (zip xs ys)

scalarProd :: Num a => [a] -> [a] -> a
scalarProd xs ys = sum (map (\(x, y) -> x * y ) (zip xs ys))

{-
  Зад. 3. Напишете функция getDecreasing, която за даден списък xs, елементите на който са
  непразни списъци от числа, връща като резултат списък от тези елементи на xs, които
  представляват строго намаляваща редица.

  Пример:
   getDecreasing [[5, 1, 2, 3, 4], [1, 1, -1, -2, -3], [4, 3, 2, 1], [7, 6, 5]] 
   -> [[4, 3, 2, 1], [7, 6, 5]]
-}

getDecreasing :: Ord a => [[a]] -> [[a]]
getDecreasing xss = undefined

{-
  Зад. 4. Температурно измерване се описва с типа
  
  type Measuring = (Int, Float),
  
  където стойността от тип Int задава ден от месеца, а стойността от тип Float - измерена
  температура за този ден. Напишете функция closestToAverage :: [Measuring] -> Int , която по
  списък от температурни измервания намира деня, в който измерената температура е най-близо
  до средната температура през месеца.

  Примери :
    closestToAverage [(5, 18.5),(15, 13.2),
                      (10, 16.8),(11, 22.8),
                      (3, 14.6)] → 10 (средната температура е 17.18)

    closestToAverage [(1, 23.6),(6, 24.2),
                      (11, 24.2),(16, 21.2),
                      (21, 23.8),(26, 26.5),
                      (31, 24.5)] → 6 или 11 или 21 (средната температура е 24.0)
-}
type Measuring = (Int, Float)
m :: Measuring
m = (1, 2.2)

closestToAverage :: [Measuring] -> Int
closestToAverage = undefined

{-
  Зад. 5. Дефинирайте функция maxSquare xs, където xs е непразен списък от
  едноаргументни числови функции. Оценката на обръщението към функцията да е
  числова функция на един аргумент x, която дава стойността f(x) на тази
  функция f от списъка xs, за която числото f(x)^2 е най-голямо.
  
  Пример: (maxSquare [sqrt, (\x -> x + 2), (\x -> 2 * x)]) 5 -> 10
-}
maxSquare :: (Ord a, Num a) => [(a -> a)] -> (a -> a)
maxSquare xs = undefined

{-
  Зад. 6. Поръчка се описва с типа type Order = (String, Int) , като стойността
  от тип String задава името на продукт, а стойността от тип Int - поръчаното
  количество от този продукт. Напишете функция mostPopular :: [Order] -> String,
  която по даден списък от поръчки намира името на продукта, от който са поръчани
  най-много бройки.

  Примери :
    mostPopular [("Mouse", 3), ("Keyboard", 3),
                 ("Monitor", 2), ("Monitor", 4),
                 ("Mouse", 1)] → "Monitor"

    mostPopular [("Mouse", 3), ("Keyboard", 3),
                 ("Monitor", 2), ("Monitor", 4),
                 ("Web camera", 4),("Keyboard", 1)]
    → "Monitor" или "Web camera"
-}



main :: IO()
main = do
    print (1)
  
  --Задача 1:
    print (isHeterogram "abcd")
    print (isHeterogram "abbd")
  
  --Задача 2:
    print (vectSum [1, 2, 3] [4, 5, 6])
    print (scalarProd [1, 2, 3] [4, 5, 6])
  
  --Задача 3:
    print (getDecreasing [[5, 1, 2, 3, 4], [1, 1, -1, -2, -3], [4, 3, 2, 1], [7, 6, 5]])
  
  --Задача 4:
    --print (closestToAverage [(5, 18.5), (15, 13.2), (10, 16.8), (11, 22.8), (3, 14.6)])
    --print (closestToAverage [(1, 23.6), (6, 24.2), (11, 24.2), (16, 21.2), (21, 23.8), (26, 26.5), (31, 24.5)])
                         
  --Задача 5:
    print (maxSquare [sqrt, (\x -> x + 2), (\x -> 2 * x)] 5)
  
  --Задача 6:
    --print (mostPopular [("Mouse", 3), ("Keyboard", 3), ("Monitor", 2), ("Monitor", 4), ("Mouse", 1)])
    --print (mostPopular [("Mouse", 3), ("Keyboard", 3), ("Monitor", 2), ("Monitor", 4), ("Web camera", 4), ("Keyboard", 1)])