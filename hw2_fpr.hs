import Data.Char
import Data.List

--Задача 1:
normalize :: String -> String
normalize "" = ""
normalize (x:xs) = if isDigit x 
                   then error "Contain digit" 
                   else if isAlpha x 
                         then toUpper x : normalize xs 
                         else normalize xs 
   
--Задача 2:   
enumerate :: Integral b => [a] -> [(b, a)]
enumerate xs = zip [0..] xs

indices :: Eq a => a -> [a] -> [Int]
indices x xs = [i | (i, y) <- enumerate xs, x == y]

lastIndex :: Char -> [Char] -> Int
lastIndex x xs = if x `elem` xs
                 then last (indices x xs)
                 else 0

returnElemFromIndex :: [Char] -> Int -> Char
returnElemFromIndex [] _ = '0'
returnElemFromIndex (x:xs) i | i <= 0 = x
                             | otherwise = returnElemFromIndex xs (i - 1)

--Задача 2.а):
encode :: [Char] -> Char -> Int -> Char
encode alphabet ch offset = 
        if ch `elem` alphabet
        then returnElemFromIndex alphabet pos
        else error "Unsupported symbol"
            where i = if offset > length alphabet || offset < - length alphabet
                      then offset - (length alphabet) * (offset `rem` length alphabet)
                      else offset
                  pos | (lastIndex ch alphabet) + i > length alphabet = (lastIndex ch alphabet) + i - length alphabet
                      | (lastIndex ch alphabet) + i < 0 = length alphabet + (lastIndex ch alphabet) + i
                      | otherwise = (lastIndex ch alphabet) + i

--Задача 2.б):
encrypt :: [Char] -> Int -> String -> [Char]
encrypt alphabet 0 _ = []
encrypt alphabet offset "" = []
encrypt alphabet offset (x:xs) = (encode alphabet x offset):(encrypt alphabet offset xs)

--Задача 2.в):
decrypt :: [Char] -> Int -> String -> [Char]
decrypt alphabet offset encrypted = encrypt alphabet (-offset) encrypted

--Задача 3.а):
crackall :: [Char] -> String -> [[Char]]
crackall alphabet encrypted = helper 1 where
                        helper i | i == length encrypted - 1 = []
                                 | otherwise = decrypt alphabet i encrypted : helper (i + 1)

--Задача 3.б):
substring :: [Char] -> [Char] -> Bool
substring _ [] = False
substring sub str = isInfixOf' sub str

isInfixOf' :: (Eq a) => [a] -> [a] -> Bool
isInfixOf' sub str = any (isPrefixOf sub) (tails str)

--Задача 3.в)
substringList :: [String] -> String -> Bool
substringList sub encrypted = or (map (\x -> substring x encrypted) sub)

crackcandidates :: [Char] -> [String] -> [Char] -> [String]
crackcandidates alphabet commonwords encrypted = filter (substringList commonwords) (xs) 
                                                         where xs = crackall alphabet encrypted

--Задача 4.а):
polyencrypt :: [Char] -> Int -> Int -> Int -> String -> String
polyencrypt alphabet offset step blockSize [] = []
polyencrypt alphabet offset step blockSize normalized = encrypt alphabet offset (take blockSize normalized) ++ 
                                                polyencrypt alphabet (offset + step) step blockSize (drop blockSize normalized)
         
--Задача 4.б):
polydecrypt :: [Char] -> Int -> Int -> Int -> String -> String
polydecrypt alphabet offset step blockSize encrypted = polyencrypt alphabet (-offset) (-step) blockSize encrypted

--Задача 5.а):
type offset = Int
type step = Int
type blockSize = Int
type rotors = (offset, step, blockSize)

enigmaencrypt :: [Char] -> [rotors] -> [Char] -> [Char]
enigmaencrypt alphabet [] normalized = normalized
enigmaencrypt alphabet (x, y, z) normalized = enigmaencrypt alphabet (x, y, z) (polyencrypt alphabet x y z normalized)
    
--Задача 5.б):
enigmadecrypt :: [Char] -> [rotors] -> [Char] -> [Char]
enigmadecrypt alphabet [] normalized = normalized
enigmadecrypt alphabet (x, y, z) normalized = enigmadecrypt alphabet (x, y, z) (polydecrypt alphabet x y z normalized)


main :: IO ()
main = do
   -- print (normalize "attack London tomorrow at ten a.m.")
   -- print (normalize "Attack London tomorrow at 10 a.m.")
    
   -- print (encode ['A'..'Z'] 'A' 1)  --B
   -- print (encode ['A'..'Z'] 'C' 2)  --E
   -- print (encode ['A'..'Z'] 'Z' 3)  --C
   -- print (encode ['A'..'Z'] 'A' (-1))  --Z
   -- print (encode ['A'..'Z'] 'C' (-2))  --A
   -- print (encode ['A'..'Z'] 'Z' (-3))  --W 
   -- print (encode ['A'..'Z'] '@' 1) -- = error “unsupported symbol: @”
    
   -- print (encrypt ['A'..'Z'] 5 "ATTACKLONDONTOMORROWATTENAM") --"FYYFHPQTSITSYTRTWWTBFYYJSFR"
   -- print (decrypt ['A'..'Z'] 6 "FYYFHPQTSITSYTRTWWTBFYYJSFR") --"ATTACKLONDONTOMORROWATTENAM"
   -- print (crackall ['A'..'Z'] "FYYFHPQTSITSYTRTWWTBFYYJSFR")
    
   -- print (substring "Haskell" "Haskell Curry") -- True
   -- print (substring "Curry" "Haskell Curry") -- True
   -- print (substring "Turing" "Haskell Curry") -- False
   
   -- print (crackcandidates ['A'..'Z'] ["THE","AND","AT","ON","IS"] "FYYFHPQTSITSYTRTWWTBFYYJSFR")
   -- print (subStrList ["THE","AND","IS"] "ATTACKLONDONTOMORROWATTENAM")
   
   -- print (polyencrypt ['A'..'Z'] 5 1 7 "ATTACKLONDONTOMORROWATTENAM")
   -- print (polydecrypt ['A'..'Z'] 5 1 7 "FYYFHPQUTJUTZUTVYYVDHBBMVIU")
   
   print (enigmaencrypt ['A'..'Z'] [(5,1,1),(7,2,10),(13,3,25)] "ATTACKLONDONTOMORROWATTENAM")
  -- print (enigmadecrypt ['A'..'Z'] [(5,1,1),(7,2,10),(13,3,25)] "ZTUCFOQUULZZGCBEIJHQXRSEOFS")
   