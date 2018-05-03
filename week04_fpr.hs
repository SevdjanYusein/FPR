{- Задачи -}

lastDigit :: Integer -> Integer
lastDigit n = n `mod` 10

firstDigits :: Integer -> Integer
firstDigits n = n `div` 10

numDigits :: Integer -> Integer
numDigits n =
    if n < 10 
    then 1
    else 1 + numDigits (firstDigits n)

{-
Задача 1. Да се дефинира функцията listLength xs, която приема списък xs и
връща неговата дължина (подобно на функцията length от стандартната Прелюдия).

Примери:
    listLength [] = 0
    listLength [1, 2, 3, 4] = 4
-}
listLength :: [a] -> Int
listLength xs 
    | null xs = 0
    | otherwise = 1 + listLength (tail xs)


{-
Задача 2. Да се дефинира функцията isElementOf x xs, която приема дадено число x
и списък от числа xs и връща дали x се съдържа в xs (подобно на функцията elem).

Примери:
    3 `isElementOf` [1, 2, 3, 4] = True
    5 `isElementOf` [1, 2, 3, 4] = False
-}
isElementOf :: Integer -> [Integer] -> Bool
isElementOf x xs 
    | null xs = False
    | head xs == x = True
    | otherwise = isElementOf x (tail xs)


{-
Задача 3. Да се дефинира функция interval a b, която връща списък с числата в
интервала [a .. b] (за целта НЕ може да използвате израза [a .. b]).

Примери:
    interval 1 4 = [1, 2, 3, 4]
    interval 2 5 = [2, 3, 4, 5]
    interval 5 2 = []
-}
interval :: Integer -> Integer -> [Integer]
interval a b 
    | a > b = []
    | otherwise = [a] ++ interval (a + 1) b
   -- a:interval (a+1) b 
{-
Задача 4. Да се дефинира функция digits n, която връща списък с цифрите на
цялото число n >= 0.

Примери:
    digits 1234 = [1, 2, 3, 4]
    digits 1750 = [1, 7, 5, 0]
-}
digits :: Integer -> [Integer]
digits n 
    | n < 10 = [n]
    | otherwise = digits (firstDigits n) ++ [lastDigit n]
{-
Задача 5. Да се дефинира функция removeDuplicates xs, която приема списък от
числа xs, и връща списък от числа, в който са премахнати всички дупликати в xs.

Примери:
    removeDuplicates [1, 2, 1] = [1, 2]
    removeDuplicates [1, 3, 7, 3, 5, 1] = [1, 3, 7, 5]
-}
removeDuplicates :: [Integer] -> [Integer]
removeDuplicates xs 
    | null xs = []
    | head xs `elem` tail xs = removeDuplicates (tail xs)
    | otherwise = head xs : removeDuplicates (tail xs)
    ramDup' xs = reverse (removeDuplicates (reverse xs))


{-
Задача 6*. Mergesort: mergesort e един от най-ефикасните алгоритми за сортиране, особено що се отнася до
функционални езици. Функцията sort от модула Data.List, която е стандартния начин за сортиране на списъци
в Haskell, използва - макар и малко по-оптимизиран от този, който ще имплементирате - вариант на този алгоритъм.

а). Нaпишете функцията merge xs ys, която приема два списъка подредени в нарастващ ред и ги
обединява в един списък, чийто елементи също са подредени в нарастващ ред.

Пример:
    merge [1, 3, 7] [2, 4, 6] = [1, 2, 3, 4, 6, 7]

б). Използвайте функцията от предишната подточка и идеята, че мога да сортирам списък като го
разделя на две половини, сортирам всяка от тях поотделно и после ги обединя - което е пример за
т. нар. подход на разделяй и владей (divide and conquer) - за да напишете функция mergesort xs,
която приема списък xs и връща списък с елементите на xs сортирани в нарастващ ред.

Пример:
    mergesort [2, 1, 3, 7, -16, 5] = [-16, 1, 2, 3, 5, 7]
-}
-- Ord a => [a] ... означава, че елементите на списъците са сравними по-между си
-- (или че съществува наредба - Ordering - между тях).
merge :: Ord a => [a] -> [a] -> [a]
merge xs ys 
    | null xs   = ys
    | null ys   = xs
    | x <= y    = x:merge xs' ys
    | otherwise = y:merge xs ys' where
        x = head xs; xs' = tail xs
        y = head ys; ys' = tail ys


mergesort :: Ord a => [a] -> [a]
mergesort xs 
    | length xs < 2 = xs
    | otherwise     = merge (mergesort hs) (mergesort ts) where
        (hs, ts) = splitAt (length xs `div` 2) xs



-- примери от условията на задачите
main = do
    -- Задача 1.
    print (listLength [])
    print (listLength [1, 2, 3, 4])

    -- Задача 2.
    print (3 `isElementOf` [1, 2, 3, 4])
    print (5 `isElementOf` [1, 2, 3, 4])

    -- Задача 3.
    print (interval 1 4)
    print (interval 2 5)
    print (interval 5 2)

    -- Задача 4.
    print (digits 1234)
    print (digits 1750)

    -- Задача 5.
    print (removeDuplicates [1, 2, 1])
    print (removeDuplicates [1, 3, 7, 3, 5, 1])

    -- Задача 6.
    print (merge [1, 3, 7] [2, 4, 6])
    print (mergesort [2, 1, 3, 7, -16, 5])