module VarsExistentes where

import GrowTree
import Data.List


-- primeiro pegar em todas as variaveis que existem na arvore
-- depois juntar em elementos diferentes tipo [[x^2, 5*x^2, 7*x^2], [y^2, 10*y^2]]
-- depois fazer a simplificação da soma [[13*x^2], [11*y^2]]
-- x^2 + y^2 + 5*x^2


-- retorna as variaveis existentes em cada termo do polinomio
varEx :: Arv a -> [[String]]
varEx a =  remDup [sort x | x <- varExSoma a]
        -- poten = somaExp (varExProd ) lTermos (populateList (length vars))


-- remove duplicados e coloca por ordem, ie: em vez de ter [x, x, x, y, y, x ,x ,z, a] só tem [a, x, y, z]
remDup :: [[String]] -> [[String]]
remDup l = map head (group (sort l))

-- separa as variaveis por termo
varExSoma :: Arv a -> [[String]]
varExSoma (NoSoma x l r) = (varExSoma l) ++ (varExSoma r)

varExSoma a = [zipWith (++) vars (zipWith (++) (take n (repeat "^")) exp)]
    where
        vars = varExProd a
        n = length vars
        exp = [show x | x <- somaExp vars a (populateList2 n)]

-- retorna as variaveis dentro de um termo
varExProd :: Arv a -> [String]
varExProd (NoProd x l r) = (varExProd l) ++ (varExProd r)
varExProd (NoPoten x (NoVar l) (NoNum r)) = [l]
varExProd a = []



-- acumula os expoentes de um termo numa lista de ints que corresponde a sua posicao/variavel
-- [x, y], arvore com tudo -> procura pela arvore pelos numeros que correspondem
somaExp :: [String] -> Arv a ->[Int] -> [Int]
somaExp (x:xs) t l = somaExp xs t (insertInIndex2 id l coef)
    where
        coef = (foldr (+) 0 (findNoPoten t x))
        id = (myFindIndex2 x (varExProd t) 0)


-- encontra os todos os expoentes de uma dada variavel dentro de um termo 
findNoPoten :: Arv a -> String -> [Int]
findNoPoten (NoPoten x (NoVar l) (NoNum r)) s
    | l==s = [r] 
    | otherwise = [0]

findNoPoten (NoProd x l r) s = (findNoPoten l s) ++ (findNoPoten r s)
findNoPoten a s= [0]


-- acumula no indice correspondente a posicao da variavel
insertInIndex2 :: Int -> [Int] -> Int -> [Int]
insertInIndex2 i (x:xs) n
    | i == 0 = [x+n] ++ xs
    | otherwise = x : insertInIndex2 (i-1) xs n


-- encontra o indice correspondente a posicao da variavel
myFindIndex2 :: String -> [String] -> Int -> Int
myFindIndex2 s (x:xs) i
    | s==x = i
    | otherwise = myFindIndex2 s xs (i+1)


populateList2 :: Int -> [Int]
populateList2 n = take n (repeat 0) 