module NormExp where

import GrowTree
import MinceString
import VarsExistentes

import Data.List
-- Pegar num NoSoma -> (NoSoma x l r)
-- Ver ls e rs
-- Ver o left e right de ls e rs



-- concatenar os prods com *
-- concatenar com o coef com *
-- concatenar tudo com +
normPoli :: Arv a -> String
normPoli Vazia = ""
normPoli a = simpIndep (juntaSoma (concatCoefs coefs (concatVars vars)))
    where
        vars = (varEx a)
        coefs = somaTermos (listaArv a) vars (populateList (length vars))


-- retorna a soma simplificada de cada tipo de termo : x + 3*x + x = 5*x
somaTermos :: [Arv a] -> [[String]] -> [Int] -> [Int]
somaTermos [] p l = l
somaTermos (x:xs) p l = somaTermos xs p (insertInIndex id l coef)
    where 
        coef = (foldr (*) 1 (findNoNum x))
        id = (myFindIndex1 (varExProd x) p 0)

listaArv :: Arv a -> [Arv a]
listaArv (NoSoma x l r) = (listaArv l) ++ (listaArv r)
listaArv a = [a]


insertInIndex :: Int -> [Int] -> Int -> [Int]
insertInIndex i (x:xs) n
    | i == 0 = [x+n] ++ xs
    | otherwise = x : insertInIndex (i-1) xs n


populateList :: Int -> [Int]
populateList n = take n (repeat 0) 


findNoNum :: Arv a -> [Int]
findNoNum (NoNum a) = [a]
findNoNum (NoProd x l r) = (findNoNum l) ++ (findNoNum r)
findNoNum a = [1]


myFindIndex1 :: [String] -> [[String]] -> Int -> Int
myFindIndex1 s (x:xs) i
    | s==x = i
    | otherwise = myFindIndex1 s xs (i+1)


concatVars :: [[String]] -> [String]
concatVars [] = []
concatVars (x:xs) = (concatVarsAux x) : (concatVars xs)

concatVarsAux :: [String] -> String
concatVarsAux [] = []
concatVarsAux [a] = a
concatVarsAux (x:xs) = x ++ "*" ++ (concatVarsAux xs)

concatCoefs :: [Int] -> [String] -> [String]
concatCoefs coef var = zipWith (++) (zipWith (++) c ((take n (repeat "*")))) var
    where 
        n = length coef
        c = [show x | x<-coef]


juntaSoma :: [String] -> String
juntaSoma [a] = a
juntaSoma (x:xs) = x ++ " + " ++ (juntaSoma xs)

simpIndep :: String -> String
simpIndep [] = []
simpIndep (x:xs)
    | x == '*' && (xs == "" || (head xs) == ' ')  = xs
    | otherwise = x : (simpIndep xs)








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
 


