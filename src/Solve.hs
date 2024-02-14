module Solve (solve_1, solve_2) where

import Brute (bruteForce)
import Genetic (geneticAlg, initialGens, congruentialGenerator, genEmptyGen)
import Support (getValue, getItems, getLength, divideByTwo, reverseList, listToString, square)

-- ZPRACOVANI STDIN
solve_1 :: String -> [String] -> IO ()
solve_1 p strings = do
    contents <- getContents
    let file = lines contents
    if checkFile file strings then solve p file else putStrLn "Invalid form of input!"

-- ZPRACOVANI SOUBORU
solve_2 :: String -> String -> [String] -> IO ()
solve_2 p s strings = do
    contents <- readFile s
    let file = lines contents
    if checkFile file strings then solve p file else putStrLn "Invalid form of file!"

-- KONTROLA FORMATU VSTUPU
checkFile :: [String] -> [String] -> Bool
checkFile [] _ = False
checkFile _ [] = True
checkFile (file:xs) (string:ys) = checkSubstring file string && (checkFile xs ys)

-- KONTROLA SHODY ZACATKU SLOVA S PODSLOVEM  
checkSubstring :: String -> String -> Bool
checkSubstring _ [] = True
checkSubstring [] _ = False 
checkSubstring (x:xs) (y:ys) = if x == y then checkSubstring xs ys else False

-- HLEDANI RESENI
solve :: String -> [String] -> IO ()
solve p text = do
    let maxWeight = read $ getValue (text !! 1)
    let minCost = read $ getValue (text !! 2)
    let items = reverseList (getItems (take ((getLength text) - 6) (drop 4 text)))
    case p of
        "-i"    -> putStrLn $ listToString text
        "-b"    -> putStrLn $ bruteForce items minCost maxWeight 1 (divideByTwo (getLength items)) 0 0 (square 2 (divideByTwo (getLength items)))
        "-o"    -> putStrLn $ geneticAlg items (initialGens 10 (items !! 0) (divideByTwo (getLength items))) minCost maxWeight 1000 (divideByTwo (getLength items)) (congruentialGenerator (items !! 0)) (genEmptyGen (divideByTwo (getLength items))) 0 False