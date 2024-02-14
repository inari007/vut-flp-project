module Brute (bruteForce) where

import Data.Bits

-- BRUTE FORCE ALGORITMUS, JEDNOTLIVE PRIPADY REPREZENTAVANY BITY PROMENNE o (NAPR.: o=10 -> 1010 (OBSAHUJE PREDMETY 1 A 3))
bruteForce :: [Int] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> String
bruteForce items min max o len best best_val amount
    | o <= amount = if checkOption (getWeightCost items o 1 0 0) (max, min) > best_val then bruteForce items min max (o + 1) len o (snd (getWeightCost items o 1 0 0)) amount else bruteForce items min max (o + 1) len best best_val amount
    | otherwise = if checkOption (getWeightCost items best 1 0 0) (max, min) > 0 then "Solution [" ++ (init (printOption best len 1)) ++ "]" else "False"

-- VYPIS RESENI
printOption :: Int -> Int -> Int -> String
printOption o len m
    | len == 0 = ""
    | (.&.) o m == m = "1 " ++ (printOption o (len-1) (m*2))   
    | otherwise = "0 " ++ (printOption o (len-1) (m*2))

-- KONTROLA, JESTLI RESENI SPLNUJE PREDPOKLADY
checkOption :: (Int, Int) -> (Int, Int) -> Int
checkOption (s, c) (max_s, min_c)
    | s <= max_s = if c >= min_c then c else 0
    | otherwise = 0

-- ZISKANI DVOJCE (VAHA, SUMA) POMOCI BITOVE OPERACE AND
getWeightCost :: [Int] -> Int -> Int -> Int -> Int -> (Int, Int)
getWeightCost [] _ _ s c = (s, c)
getWeightCost (x:y:xs) o m s c
    | (.&.) o m == m = getWeightCost xs o (m*2) (s+x) (c+y)
    | otherwise = getWeightCost xs o (m*2) s c    