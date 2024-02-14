module Support (getValue, getItems, getLength, divideByTwo, append, reverseList, listToString, square) where

import Data.Char

-- ZISKANI HODNOTY NA RADKU VSTUPU
getValue :: String -> String
getValue [] = ""
getValue (x:xs)
        | isNumber x = [x] ++ getValue xs
        | otherwise = getValue xs

-- ZISKANI HODNOT PREDMETU ZE SOUBORU/STDIN
getItems :: [String] -> [Int]
getItems [] = []
getItems (x:xs)
    | getValue x /= "" = append (read (getValue x)) (getItems xs)
    | otherwise = getItems xs

-- VYPOCET DELKY LISTU
getLength :: [a] -> Int
getLength [] = 0
getLength (x:xs) = 1 + getLength xs

-- FANCY DELENI DVEMA
divideByTwo :: Int -> Int
divideByTwo n
    | n <= 1 = 0
    | otherwise = 1 + divideByTwo (n-2)

-- VLOZI PRVEK NA KONEC LISTU
append :: a -> [a] -> [a]
append a [] = [a]
append a (x:xs) = x : append a xs

-- REVERZACE LISTU
reverseList :: [Int] -> [Int]
reverseList [] = []
reverseList (x:xs) = append x (reverseList xs)

-- PREDZPRACOVANI ('SERIALIZACE') LISTU PRO VYPISOVE FUNKCE
listToString :: [String] -> String
listToString [] = ""
listToString (x:xs) = x ++ "\n" ++ (listToString xs)

-- CELOCISELNA MOCNINA
square :: Int -> Int -> Int
square _ 0 = 1 
square x y = x * (square x (y-1))