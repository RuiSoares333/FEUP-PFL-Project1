module NormExp where
import GrowTree
import Utils
import VarsExistentes
import MinceString




normPoli :: Arv a-> String
normPoli Vazia = ""
normPoli a = simpIndep (juntaSoma (concatCoefs coefs (concatVars vars)))
    where
        vars = varEx a
        coefs = somaTermos (listaArv a) vars (populateList (length vars))


-- retorna a soma simplificada de cada tipo de termo : x + 3*x + x = 5*x
somaTermos :: [Arv a] -> [[String]] -> [Int] -> [Int]
somaTermos [] p l = l
somaTermos (x:xs) p l = somaTermos xs p (insertInIndex id l coef)
    where
        coef = foldr (*) 1 (findNoNum x)
        id = myFindIndex (head (varExSoma x)) p 0



listaArv :: Arv a -> [Arv a]
listaArv (NoSoma x l r) = listaArv l ++ listaArv r
listaArv a = [a]



findNoNum :: Arv a-> [Int]
findNoNum (NoNum a) = [a]
findNoNum (NoProd x l r) = findNoNum l ++ findNoNum r
findNoNum a = [1]



concatVars :: [[String]] -> [String]
concatVars = map concatVarsAux



concatVarsAux :: [String] -> String
concatVarsAux [] = []
concatVarsAux [a] = a
concatVarsAux (x:xs) = x ++ "*" ++ concatVarsAux xs



concatCoefs :: [Int] -> [String] -> [String]
concatCoefs coef var = zipWith (++) finalCoef var
    where
        n = length coef
        c = [show x | x<-coef]
        finalCoef = zipWith (++) c (replicate n "*")



juntaSoma :: [String] -> String
juntaSoma [a] = a
juntaSoma (x:xs) = x ++ " + " ++ juntaSoma xs



simpIndep :: String -> String
simpIndep [] = []
simpIndep (x:xs)
    | x == '*' && (xs == "" || head xs == ' ')  = xs
    | otherwise = x : simpIndep xs
