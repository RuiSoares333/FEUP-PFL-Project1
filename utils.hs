-- TODO Funçoes comuns
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Utils where
import Data.List

-- acumula no indice correspondente a posicao da variavel
insertInIndex :: Int -> [Int] -> Int -> [Int]
insertInIndex i (x:xs) n
    | i == 0 = (x+n) : xs
    | otherwise = x : insertInIndex (i-1) xs n


populateList :: Int -> [Int]
populateList n = replicate n 0


myFindIndex :: (Eq a) => a -> [a] -> Int -> Int
myFindIndex s (x:xs) i
    | s==x = i
    | otherwise = myFindIndex s xs (i+1)

remDup :: (Eq a, Ord a) => [a] -> [a]
remDup l = map head (group (sort l))
