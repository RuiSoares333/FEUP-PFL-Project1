module Main where

import GrowTree ( paraArv, Arv(..) )
import MinceString ( parseString )
import NormExp ( normPoli )

main = do
    putStrLn "AAAAAAAAA"





--------------------------------------------------------- SOMA ---------------------------------------------------------

-- recebe 2 polinomios
-- retorna a soma dos 2 polinomios
sumPoly :: String -> String -> String
sumPoly a b = normPoli (paraArv (parseString (a ++ "+" ++ b)))





--------------------------------------------------------- PRODUTO ---------------------------------------------------------

-- recebe 2 polinomios 
-- retorna o produto dos dois
multPoly :: String -> String -> String
multPoly _ "" = ""
multPoly "" _ = "" 
multPoly a b = normPoli (juntaNoSoma (juntaAllProd (getTermos arvA) (getTermos arvB)))
    where
        arvA = paraArv (parseString a)
        arvB = paraArv (parseString b)


-- recebe uma arvore que representa um polinomio
-- retorna todos os termos do polinomio
getTermos :: Arv a -> [Arv a]
getTermos (NoSoma x l r) = getTermos l ++ getTermos r
getTermos a = [a]


-- recebe 2 listas de arvores que representam os termos dos dois polinomios
-- retorna a lista de todos os termos da primeira arvore multiplicados por todos os termos da segunda arvore
juntaAllProd :: [Arv a] -> [Arv a] -> [Arv a]
juntaAllProd [] b = []
juntaAllProd (x:xs) b = juntaUmProd x b (length b) ++ juntaAllProd xs b


-- recebe uma arvore que representa um termo e uma lista de arvores que representa todos os termos de um polinomio
-- retorna o a lista com o produto do termo com todos os termos do outro polinomio
juntaUmProd :: Arv a -> [Arv a] -> Int -> [Arv a]
juntaUmProd a la n = zipWith (NoProd '*') (take n (repeat a)) la


-- recebe uma lista arvores que representa os termos do polinomio
-- retorna uma arvore com os termos ligados por NoSoma's
juntaNoSoma :: [Arv a] -> Arv a
juntaNoSoma [a] = a 
juntaNoSoma l = NoSoma '+' (juntaNoSoma (take n l))  (juntaNoSoma (drop n l)) 
    where n = length l `div` 2





--------------------------------------------------------- DERIVADA ---------------------------------------------------------

-- recebe um polinomio e a variavel a partir do qual se vai fazer a derivada
-- retorna o polinomio derivado
derivPoly :: String -> String -> String
derivPoly a s = normPoli (myDerivPoly (paraArv (parseString a)) s)


-- recebe uma arvore que representa o polinomio e a variavel a partir do qual se vai fazer a derivada
-- retorna o polinomio derivado
myDerivPoly :: Arv a -> String -> Arv a
myDerivPoly (NoPoten x (NoVar v) (NoNum e)) s
    | v == s = NoProd '*' (NoNum e) (NoPoten '^' (NoVar v) (NoNum (e-1)))
    | otherwise = NoNum 0

myDerivPoly (NoSoma x l r) s = NoSoma '+' (myDerivPoly l s) (myDerivPoly r s)
myDerivPoly (NoProd x l r) s = NoProd '*' (myDerivPoly l s) (myDerivPoly r s)
myDerivPoly a _ = a

