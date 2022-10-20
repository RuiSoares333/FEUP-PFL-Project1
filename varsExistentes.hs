module VarsExistentes where

import GrowTree ( Arv(..) )
import Data.List ( group, sort )
import Utils ( insertInIndex, populateList, myFindIndex, remDup )



-- recebe uma arvore (polinomio)
-- retorna as variaveis existentes em cada termo do polinomio
varEx :: Arv -> [[String]]
varEx a =  remDup [sort x | x <- varExSoma a]


-- recebe uma arvore (polinomio)
-- separa as variaveis por termo
-- retorna as variaveis existentes em cada termo do polinomio
varExSoma :: Arv -> [[String]]
varExSoma (NoSoma x l r) = varExSoma l ++ varExSoma r

varExSoma a = [zipWith (++) vars (zipWith (++) (replicate n "^") exp)]
    where
        vars = remDup (varExProd a)
        n = length vars
        exp = [show x | x <- somaExp vars a (populateList n)]


-- recebe uma arvore (termo)
-- retorna as variaveis dentro de um termo
varExProd :: Arv -> [String]
varExProd (NoProd x l r) = varExProd l ++ varExProd r
varExProd (NoPoten x (NoVar l) (NoNum r)) = [l]
varExProd a = []



-- recebe uma lista com as variaveis no polinomio, uma arvore (termo), e uma lista com 0's
-- acumula na lista de 0's o expoente acumulado de cada variavel no termo
-- retorna a lista com cada expoente acumulado na posicao que lhe corresponde
somaExp :: [String] -> Arv ->[Int] -> [Int]
somaExp [] _ l = l
somaExp (x:xs) t l = somaExp xs t (insertInIndex id l coef)
    where
        coef = sum (findNoPoten t x)
        id = myFindIndex x (remDup (varExProd t)) 0


-- recebe uma arvore (termo) e a variavel
-- retorna os todos os expoentes de uma dada variavel dentro de um termo 
findNoPoten :: Arv -> String -> [Int]
findNoPoten (NoPoten x (NoVar l) (NoNum r)) s
    | l==s = [r]
    | otherwise = [0]

findNoPoten (NoProd x l r) s = findNoPoten l s ++ findNoPoten r s
findNoPoten a s= [0]
