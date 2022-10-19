module Utils where

import Data.List ( group, sort )

-- recebe o indice, uma lista e um valor
-- retorna a lista com o valor adicionado no indice
insertInIndex :: Int -> [Int] -> Int -> [Int]
insertInIndex i (x:xs) n
    | i == 0 = (x+n) : xs
    | otherwise = x : insertInIndex (i-1) xs n


-- recebe o tamanho n da lista
-- retorna uma lista com n posicoes a 0
populateList :: Int -> [Int]
populateList n = replicate n 0


-- recebe um elemento, uma lista de elementos e um contador de indice
-- retorna o contador quando o elemento corresponde ao elemento atual da lista
myFindIndex :: (Eq a, Ord a) => a -> [a] -> Int -> Int
myFindIndex s (x:xs) i =
    if s == x then i
    else myFindIndex s xs (i+1)


-- recebe uma lista
-- retorna a lista sem valores duplicados
remDup :: (Eq a, Ord a) => [a] -> [a]
remDup l = map head (group (sort l))


