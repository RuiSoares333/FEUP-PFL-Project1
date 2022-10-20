module NormExp where

import GrowTree (Arv (..))
import Utils ( insertInIndex, populateList, myFindIndex )
import VarsExistentes ( varEx, varExSoma )


myNormPoly :: Arv a -> String
myNormPoly Vazia = "0"
myNormPoly a
    | res == "" = "0"
    | otherwise = res
    where
        vars = varEx a
        coefs = somaTermos (listaArv a) vars (populateList (length vars))
        res = simpIndep (juntaSoma (removePoten (concatCoefs coefs (concatVars vars))))


-- recebe os termos, as variaveis existentes, e uma lista que acumula o coeficiente de cada variavel
-- retorna a soma simplificada de cada tipo de termo : x + 3*x + x = 5*x
somaTermos :: [Arv a] -> [[String]] -> [Int] -> [Int]
somaTermos [] p l = l
somaTermos (x:xs) p l = somaTermos xs p (insertInIndex id l coef)
    where
        coef = foldr (*) 1 (findNoNum x)
        id = myFindIndex (head (varExSoma x)) p 0


-- recebe uma arvore (polinomio)
-- retorna todos os termos do polinomio
listaArv :: Arv a -> [Arv a]
listaArv (NoSoma x l r) = listaArv l ++ listaArv r
listaArv a = [a]


-- recebe uma arvore (termo)
-- retorna o coeficiente desse termo
findNoNum :: Arv a-> [Int]
findNoNum (NoNum a) = [a]
findNoNum (NoProd x l r) = findNoNum l ++ findNoNum r
findNoNum a = [1]

potenSimp :: String -> String
potenSimp s
    | sub == "^1" = take (length s-2) s
    | sub == "^0" = "1"
    | otherwise = s
    where sub = drop (length s-2) s

removePoten :: [String] -> [String]
removePoten = map potenSimp

prodUm :: String -> Bool
prodUm "1" = True
prodUm a = False

removeProdUm :: [String] -> [String]
removeProdUm [] = []
removeProdUm (x:xs)
    | prodUm x = removeProdUm xs
    | otherwise = x : removeProdUm xs

-- recebe as variaveis e coeficientes de todos os termos
-- retorna os termos normalizados
concatVars :: [[String]] -> [String]
concatVars = map (concatVarsAux . removeProdUm . removePoten)


-- recebe as variaveis e coeficentes
-- retorna um termo normalizado
concatVarsAux :: [String] -> String
concatVarsAux [] = []
concatVarsAux [a] = a
concatVarsAux (x:xs) = x ++ "*" ++ concatVarsAux xs


-- recebe a lista dos coeficientes e das variaveis
-- retorna a lista com o produto dos coeficientes com as respetivas variaveis
concatCoefs :: [Int] -> [String] -> [String]
concatCoefs coef var = concatCoefsAux2 finalCoef var
    where
        n = length coef
        c = [show x | x<- coef]
        finalCoef = concatCoefsAux1 c var

concatCoefsAux1 :: [String] -> [String] -> [String]
concatCoefsAux1 [] [] = []
concatCoefsAux1 [_] [] = [""]
concatCoefsAux1 (x:xs) (y:ys)
    | y == "" = x : concatCoefsAux1 xs ys
    | x == "1" = "" : concatCoefsAux1 xs ys
    | x == "-1" = "-" : concatCoefsAux1 xs ys
    | otherwise = (x ++ "*") : concatCoefsAux1 xs ys

concatCoefsAux2 :: [String] -> [String] -> [String]
concatCoefsAux2 [] [] = []
concatCoefsAux2 (x:xs) (y:ys)
    | x == "0*" = concatCoefsAux2 xs ys
    | y == "" = x : concatCoefsAux2 xs ys
    | otherwise = x:y : concatCoefsAux2 xs ys


-- recebe a lista com os termos
-- retorna a string com os termos concatenados com '+' ou '-' dependendo da grandeza do coeficiente
juntaSoma :: [String] -> String
juntaSoma [] = ""
juntaSoma [a] = a
juntaSoma (x:xs)
    | x == "" = juntaSoma xs
    | drop (length x-1) x == "*" = x ++ juntaSoma xs
    | (head xs /= "") && head (head xs) == '-' = x ++ " - " ++ juntaSoma (tail (head xs) : tail xs)
    | x == "-" = x ++ juntaSoma xs
    | otherwise =  x ++ " + " ++ juntaSoma xs


-- funcao auxiliar para retirar o caracter '*' do termo independente
simpIndep :: String -> String
simpIndep [] = []
simpIndep (x:xs)
    | x == '*' && (xs == "" || head xs == ' ')  = xs
    | otherwise = x : simpIndep xs
