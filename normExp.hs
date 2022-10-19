module NormExp where

import GrowTree (Arv (..))
import Utils ( insertInIndex, populateList, myFindIndex )
import VarsExistentes ( varEx, varExSoma )




-- recebe uma arvore
-- retorna a arvore normalizada e em string
normPoli :: Arv a -> String
normPoli Vazia = ""
normPoli a = simpIndep (juntaSoma (concatCoefs coefs (concatVars vars)))
    where
        vars = varEx a
        coefs = somaTermos (listaArv a) vars (populateList (length vars))


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


-- recebe as variaveis e coeficientes de todos os termos
-- retorna os termos normalizados
concatVars :: [[String]] -> [String]
concatVars = map concatVarsAux


-- recebe as variaveis e coeficentes
-- retorna um termo normalizado
concatVarsAux :: [String] -> String
concatVarsAux [] = []
concatVarsAux [a] = a
concatVarsAux (x:xs) = x ++ "*" ++ concatVarsAux xs


-- recebe a lista dos coeficientes e das variaveis
-- retorna a lista com o produto dos coeficientes com as respetivas variaveis
concatCoefs :: [Int] -> [String] -> [String]
concatCoefs coef var = zipWith (++) finalCoef var
    where
        n = length coef
        c = [show x | x<-coef]
        finalCoef = zipWith (++) c (replicate n "*")


-- recebe a lista com os termos
-- retorna a string com os termos concatenados com '+'
juntaSoma :: [String] -> String
juntaSoma [a] = a
juntaSoma (x:xs) = x ++ " + " ++ juntaSoma xs


-- funcao auxiliar para retirar o caracter '*' do termo independente
simpIndep :: String -> String
simpIndep [] = []
simpIndep (x:xs)
    | x == '*' && (xs == "" || head xs == ' ')  = xs
    | otherwise = x : simpIndep xs
