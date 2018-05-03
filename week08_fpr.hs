{-
Задача 1. Дефинирайте функцията zeta n x, която приема целочисления аргумент n и реалното число s
и връща сбора на първите n члена на редицата 1, 1 / 2 ^ x, .. 1 / k ^ x ..
-}
zeta :: Int -> Double -> Double
zeta n x | n == 1 = 1
         | otherwise = (1 / fromIntegral n ** x)  + zeta (n - 1) x


{-
Задача 2. Нека са дадени множествата xs и ys, и едноаргументата функция f от xs към ys.
Ще наричаме множеството ys образ на xs през f, тогава и само тогава когато за всяко x от xs,
f(x) принадлежи на ys и за всяко y от ys, съществува x, елемент на xs, за който f(x) = y.

Дефинирайте функцията isImageOf f xs ys, която връща дали ys е образ на xs през f.
-}
-- "преписваме" условието на задачата на Хаскел


isImageOf :: Eq b =>  (a -> b) -> [a] -> [b] -> Bool
isImageOf f xs ys = and [f x `elem` ys | x <- xs] && 
                    and [y `elem` map f xs | y <- ys]
                  -- sort (nub (map f xs)) == sort (nub ys)

{-
Задача 3. Напишете функцията argmin f xs, която връща елемента x от списъка xs, която
минимизира функцията f, т.е. това x при което f x има най-малка стойност.

Пример:
    Ако имаме people = [("Пенка", 23), ("Гошо", 19), ("Киро", 21)] и искаме да върнем наредената двойка
    от имена и възрасти с най-малка стойност, то можем да извикаме:
    
    argmin snd people                   -> ("Гошо", 19)
    argmin (\(_, age) -> age) people    -> ("Гошо", 19)
-}
argmin :: Ord b => (a -> b) -> [a] -> a
argmin f xs = head (filter (\x -> f x == minimizingX) xs) where 
                        minimizingX = minimum (map f xs)


{-
Задача 4. Нека са дефинирани следните типове:

type Temperature = Float
type Day = Int
type Month = Int
type Record = (Month, Day, Temperature)

Дефинирайте функцията coldestMonth records, която приема списък от температурни измервания
и връща, кой е бил най-студения месец (т.е. месецът с най-ниска средна температура)

Съвет: Подобен тип задачи най-лесно се решават като се разбият на подзадачи.

Пример:
    coldestMonth [
            (1, 1, 0), (1, 10, -5), (1, 20, 8),
            (2, 1, 0), (2, 10, -5), (2, 20, 0),
            (3, 1, 5), (3, 10, 10), (3, 20, 8),
            (4, 1, 9), (4, 10, 15), (4, 20, 18)
        ]
            -> 2
-}
type Temperature = Float
type Day = Int
type Month = Int
type Record = (Month, Day, Temperature)
{-
months :: [Record] -> [Month]
months records = nub (map (\(m, _, _) records))

averageTemp :: [Record] -> Temp
averageTemp records = sum (map (\ (_, _, t) -> t) records) / fromIntegral (length records)

recordsOfMonth :: [Record] -> Mont -> [Record]
recordsOfMonth records month = filter (\ (m, _, _) -> m == month) records

coldestMonth :: [Record] -> Month
coldestMonth records = argmin (\m -> averageTemp (recordsOfMonth m)) (months records) 
-}

{-
Задача 5. Дефинирайте функцията sumPrimers n, която връща сбора на първите n числа,
сумата на цифрите на които е просто число.
-}

sumPrimers :: Int -> Integer
sumPrimers n = take n (filter sumDigitsIsPrime [1..]) where
                 sumDigitsIsPrime x = isPrime (sum (digits x))
                 digits x = if x < 10 then x else digits (x `div` 10) + (x `mod` 10)


{-
Задача 6. Дефинирайте функция getDecreasing, която за даден списък xss, елементите на който са
непразни списъци от числа, връща като резултат списък от тези елементи на xss, които представляват
строго намаляваща редица.

Пример:
    getDecreasing [[5, 1, 2, 3, 4], [1, 1, -1, -2, -3], [4, 3, 2, 1], [7, 6, 5]]
                    -> [[4, 3, 2, 1], [7, 6, 5]]
-}
getDecreasing :: Ord a => [[a]] -> [[a]]
getDecreasing xss = filter isDecreasing xss  where
                        isDecreasing xs = and (zipWith (>) xs (tail xs))
                                          -- (>) <=> (\x y -> x > y)


-- примери
main :: IO ()
main = do
    -- Задача 1.
    print (zeta 1000 2)    -- ~1.64

    -- Задача 2.
    print (isImageOf (\x -> x ^ 2) [1, 2, 3, 4] [1, 4, 9, 16])     -- True
    print (isImageOf (\x -> x ^ 2) [4, 3, 2, 1] [1, 4, 9, 16])     -- True
    print (isImageOf (\x -> x ^ 2) [1, 2, 3, 4] [1, 3, 9, 16])     -- False
    print (isImageOf (\x -> x ^ 2) [1, 2, 3, 4] [1, 3, 4, 9, 16])  -- False

    print (isImageOf (\x -> 10) [1, 2, 3, 4] [10])     -- True
    print (isImageOf (\x -> 10) [1, 2, 3, 4] [1])      -- False
    print (isImageOf (\x -> 10) [1, 2, 3, 4] [1, 10])  -- False

    -- Задача 3.
    print (argmin snd [("Penka", 23), ("Gosho", 19), ("Kiro", 21)])

    -- Задача 4.
    print (coldestMonth [
            (1, 1, 0), (1, 10, -5), (1, 20, 8), 
            (2, 1, 0), (2, 10, -5), (2, 20, 0),
            (3, 1, 5), (3, 10, 10), (3, 20, 8),
            (4, 1, 9), (4, 10, 15), (4, 20, 18)
        ])

    -- Задача 5.
    print (sumPrimers 5)  -- 28  
    print (sumPrimers 6)  -- 40

    -- Задача 6.
    print (getDecreasing [[5, 1, 2, 3, 4], [1, 1, -1, -2, -3], [4, 3, 2, 1], [7, 6, 5]])