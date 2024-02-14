module Genetic (geneticAlg, initialGens, congruentialGenerator, genEmptyGen) where

import Support (append)

-- GENETIC ALGORITMUS, JEDNOTLIVE GENERACE REPREZENTOVANY POLEM BITU gens (NAPR.: [1010] -> OBSAHUJE PREDMETY 1 A 3)
-- KAZDA GENERACE MA DELKU 10
-- V RAMCI JEDNE GENERACE DVE VOLANI ->
--    1. new_gen == False, ZISKA Z 10 POTOMKU SERAZENYCH 5 NEJLEPSICH
--    2. new_gen == True, ZKONTROLUJE DOSAVADNI NEJLEPSI RESENI S NEJLEPSIM POTOMKEM A PRIPADNE HO AKTUALIZUJE, NASLEDNE VYTVORI DALSI GENERACI
geneticAlg :: [Int] -> [[Int]] -> Int -> Int -> Int -> Int -> Int -> [Int] -> Int -> Bool -> String
geneticAlg items gens min max gens_c len g best best_val new_gen
    | gens_c == 0 = if best_val >= min  then "Solution [" ++ (init (printGens best)) ++ "]" else "False"
    | new_gen == True = if checkGen (getWeightCostGen items (gens !! 0) 0 0 0) (max, min) then geneticAlg items (nextGens gens len g) min max gens_c len g (gens !! 0) (snd (getWeightCostGen items (gens !! 0) 0 0 0)) False else geneticAlg items (nextGens gens len g) min max gens_c len g best best_val False
    | otherwise = geneticAlg items (fitnessToGens (getBestFiveGens (getFitness (init (getWeightCostArr items gens)) max) 0 []) gens) min max (gens_c - 1) len (congruentialGenerator g) best best_val True

-- KONTROLA OMEZENI RESENI
checkGen :: (Int, Int) -> (Int, Int) -> Bool
checkGen (cur_s, cur_c) (max_s, min_c) = if cur_s <= max_s then (if cur_c >= min_c then True else False) else False  

-- VYTVORENI NOVE GENERACE KRIZENIM A MUTACEMI 5 NEJLEPSICH, A PRIDA 5 NAHODNYCH
nextGens :: [[Int]] -> Int -> Int -> [[Int]]
nextGens gens len g = (mutateGens (append (head gens) (crossGens (tail gens) len g)) len g) ++ (addNewGens len 5 (congruentialGenerator g))

-- VYTVORENI 4 POTOMKU KRIZENIM NEJLEPSICH (2,4) (4,2) (1,3) (3,1), UPLNE NEJLEPSI SE NEKRIZI, POUZE MUTUJE
-- NAHODNE ROZDELI POLE BITU NA 2 CASTI, A PROHODI JE  
crossGens :: [[Int]] -> Int -> Int -> [[Int]]
crossGens [] _ _ = []
crossGens (x:y:xs) len g = ((take (mod g len) x) ++ (drop (mod g len) y)) : ((take (mod g len) y) ++ (drop (mod g len) x)) : (crossGens xs len (mod (congruentialGenerator g) len))

-- MUTACE 4 NOVYCH POTOMKU A NEJLEPSIHO MINULEHO  
mutateGens :: [[Int]] -> Int -> Int -> [[Int]]
mutateGens [] _ _ = []
mutateGens (x:xs) len g =  append (changeEl x (mod (congruentialGenerator g) len) len 0) (mutateGens xs len (congruentialGenerator g))

-- NAHODNE ZMENI 1 BIT V POLI
changeEl :: [Int] -> Int -> Int -> Int -> [Int]
changeEl [] _ _ _ = []
changeEl (x:xs) i len c
    | c == i = if x == 1 then 0 : changeEl xs i len (c+1) else 1 : changeEl xs i len (c+1)
    | otherwise = x : changeEl xs i len (c+1)

-- PRIDANI 5 NAHODNYCH GENERACI
addNewGens :: Int -> Int -> Int -> [[Int]]
addNewGens len c g
    | c == 0 = []
    | otherwise =  append (genRandGen (congruentialGenerator g) len) (addNewGens len (c-1) (congruentialGenerator g))

-- VYPIS RESENI
printGens :: [Int] -> String
printGens [] = ""
printGens (x:xs) = show x ++ " " ++ (printGens xs)

-- Z INDEXU 5 NEJLEPSICH POTOMKU VRATI JEJICH BINARNI REPREZENTACE 
fitnessToGens :: [Int] -> [[Int]] -> [[Int]]
fitnessToGens [] gens = []
fitnessToGens (x:xs) gens = (gens !! x) : (fitnessToGens xs gens)  

-- VRATI INDEXY 5 NEJLEPSICH POTOMKU
getBestFiveGens :: [Int] -> Int -> [Int] -> [Int]
getBestFiveGens fitness i used
    | i < 5 = getBestFiveGens fitness (i+1) (getBestNthGen fitness used 0 0)
    | otherwise = used

-- VRATI N-NEJLEPSI RESENI 
getBestNthGen :: [Int] -> [Int] -> Int -> Int -> [Int]
getBestNthGen fitness used b c
    | c < 10 = if (fitness !! c) >= (fitness !! b) then (if elem c used then getBestNthGen fitness used b (c+1) else getBestNthGen fitness used c (c+1)) else getBestNthGen fitness used b (c+1)
    | otherwise = append b used

-- ZISKANI FITNESS FUNKCI GENERACE (MAX VAHA > VAHA -> FITNESS = 0, JINAK FITNESS = SUMA CEN)
getFitness :: [(Int, Int)] -> Int -> [Int]
getFitness [] _ = []
getFitness (x:xs) max
    | fst x > max = 0 : getFitness xs max
    | otherwise = snd x : getFitness xs max

-- ZISKANI SUMY VAH A CEN VSECH POTOMKU GENERACE
getWeightCostArr :: [Int] -> [[Int]] -> [(Int, Int)]
getWeightCostArr _ [] = [(0,0)]
getWeightCostArr items (x:xs) = (getWeightCostGen items x 0 0 0) : (getWeightCostArr items xs)

-- ZISKANI SUMY VAH A CEN JEDNOHO POTOMKA GENERACE
getWeightCostGen :: [Int] -> [Int] -> Int -> Int -> Int -> (Int, Int) -- (x:xs) [0:01]
getWeightCostGen _ [] _ s c = (s, c)
getWeightCostGen items (x:xs) i s c
    | x == 1 = getWeightCostGen items xs (i+1) (s + (items !! (i*2))) (c + (items !! ((i*2) + 1)))
    | otherwise = getWeightCostGen items xs (i+1) s c    

-- VYTVORENI NAHODNE PRVNI GENERACE
initialGens :: Int -> Int -> Int -> [[Int]]
initialGens ammount g len
    | ammount == 0 = []
    | otherwise = genRandGen (congruentialGenerator g) len : initialGens (ammount-1) (congruentialGenerator g) len

-- VYTVORENI NAHODNEHO POTOMKA
genRandGen :: Int -> Int -> [Int]
genRandGen g len
    | len > 0 = if g >= 5000 then 1 : genRandGen (congruentialGenerator g) (len-1) else 0 : genRandGen (congruentialGenerator g) (len-1)
    | otherwise = []

-- VYTVORENI PRAZDNE GENERACE (BATOH BEZ PREDMETU)
genEmptyGen :: Int -> [Int]
genEmptyGen 0 = []
genEmptyGen x = 0 : genEmptyGen (x-1)

-- GENERATOR PSEUDO-NAHODNYCH CISEL
congruentialGenerator :: Int -> Int
congruentialGenerator x = mod (x * 69069 + 13001) 10000