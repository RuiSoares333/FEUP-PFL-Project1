import GrowTree
import MinceString
import VarsExistentes

import Data.List
-- Pegar num NoSoma -> (NoSoma x l r)
-- Ver ls e rs
-- Ver o left e right de ls e rs


normPoli :: Arv a -> Arv a
normPoli Vazia = Vazia
normPoli (NoNum a) = a
normPoli a = separaPoli (trataString ( juntaSoma (zip somaTermos )))


somaTermos :: Arv a -> [[String]] -> [Int]
somaTermos (NoNum a) p = insertInIndex 0 [] p

somaTermos (NoSoma x l r) p = (somaTermos l p) ++ (somaTermos r p)
-- somaTermos (NoProd x l r) p = 

insertInIndex :: Int -> [Int] -> Int -> [Int]
insertInIndex i (x:xs) n
    | i == 0 = [x+n] ++ xs
    | otherwise = x : insertInIndex (i-1) xs n



juntaSoma :: [String] -> String
juntaSoma [] = ""
juntaSoma (x:xs) = x ++ juntaSoma xs

normSumPoli :: Arv a -> Arv a -> Arv a
normsumPoli Vazia Vazia = Vazia
normsumPoli Vazia a = a
normsumPoli a Vazia = a
normSumPoli a b = Vazia


-- normalizar a soma entre 2 termos
normProdPoli :: Arv a -> Arv a -> Arv a
normProdPoli Vazia Vazia = Vazia
normProdPoli Vazia a = a
normProdPoli a Vazia = a
normProdPoli (NoProd x lx rx) (NoProd y ly ry) 
    | checkPowEqual rx ry = (NoProd '*' (sumNoNum lx ly) (rx))
    | otherwise = NoSoma '+' (NoProd x lx rx) (NoProd y ly ry)

-- soma 2 NoNum
sumNoNum :: Arv a -> Arv a -> Arv a
sumNoNum (NoNum a) (NoNum b) = NoNum (a+b)

-- verifica se 2 NoPoten sao iguais
checkPowEqual :: Arv a -> Arv a -> Bool
checkPowEqual (NoPoten x lx rx) (NoPoten y ly ry) = (checkNoVarEqual lx ly && checkNoNumEqual rx ry)

-- verifica se 2 NoVar sao iguais
checkNoVarEqual :: Arv a -> Arv a -> Bool
checkNoVarEqual (NoVar a) (NoVar b) = a == b 

-- verifica se 2 NoNum sao iguais
checkNoNumEqual :: Arv a -> Arv a -> Bool
checkNoNumEqual (NoNum a) (NoNum b) = a == b 
