-- Задача1:
solveQuadratic :: Double -> Double -> Double -> (Double, Double)
solveQuadratic 0 _ _ = error "'a' must be different from 0"
solveQuadratic a b c = if dis >= 0
                       then (((- b + sqrt dis) / (2 * a)) , ((- b - sqrt dis) / (2 * a)))
                       else error "'D' must be positive or 0"
                            where dis = (b ^ 2) - (4 * a * c)
           
           
--Задача2:
sumPrimes :: Integer -> Integer -> Integer
sumPrimes _ 0 = 0                              -- дъно-> когато се достигне максималния брой k връщаме 0
sumPrimes n k 
    | isPrime n = n + sumPrimes (n + 1) (k - 1) 
    | otherwise = sumPrimes (n + 1) k

isPrime :: Integer -> Bool
isPrime n = n >= 2 && helper 2 where        
    helper i                           -- с helper проверяваме дали n се дели на друго число освен на 1 и на себе си
       | i > sqrtn = True
       | n `mod` i == 0 = False
       | otherwise = helper (i + 1)
    sqrtn = floor (sqrt (fromIntegral n))


--Задача3:
countPalindromes :: Integer -> Integer -> Integer
countPalindromes a b 
    | a > b = 0
    | a <= b = if isPalindrome a 
               then 1 + countPalindromes (a+1) b 
               else countPalindromes (a+1) b

isPalindrome n = if (n == reverseDigits n) 
                 then True 
                 else False

lastDigit :: Integer -> Integer
lastDigit n = n `mod` 10

firstDigits :: Integer -> Integer
firstDigits n = n `div` 10

reverseDigits :: Integer -> Integer
reverseDigits n = helper 0 n where 
    helper result number = 
       if number == 0
       then result
       else helper (10 * result + lastDigit number) (firstDigits number)

--Задача4
truncatablePrime :: Integer -> Bool
truncatablePrime 0 = True
truncatablePrime n = if isPrime n 
                     then truncatablePrime (firstDigits n)
                     else False


main = do
--Зад1
-- print(solveQuadratic 0 1 2)  -- a=0
-- print(solveQuadratic 1 1 1)  -- dis<0
 print(solveQuadratic 1 1 (-1)) -- dis>0
 print(solveQuadratic 2 4 2)    -- dis=0

--Зад2
 print(sumPrimes 1 3)

--Зад3
 print(countPalindromes 10 25)

--Зад4
 print(truncatablePrime 3797)
 print(truncatablePrime 47)
