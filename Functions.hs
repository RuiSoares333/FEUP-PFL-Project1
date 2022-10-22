module Functions where

import GrowTree ( paraArv, Arv(..) )
import MinceString ( parseString )
import NormExp( concatCoefs, concatVars, juntaSoma, listaArv, simpIndep, somaTermos, myNormPoly)
import VarsExistentes ( varEx )
import Utils ( populateList )


--------------------------------------------------------- NORMALIZAR ---------------------------------------------------------

-- recebe um polinomio
-- retorna o polinomio normalizado
normPoly :: String -> String
normPoly a = myNormPoly (paraArv (parseString a))





--------------------------------------------------------- SOMA ---------------------------------------------------------

-- recebe 2 polinomios
-- retorna a soma dos 2 polinomios
sumPoly :: String -> String -> String
sumPoly a b = normPoly (a ++ "+" ++ b)





--------------------------------------------------------- PRODUTO ---------------------------------------------------------

-- recebe 2 polinomios 
-- retorna o produto dos dois
multPoly :: String -> String -> String
multPoly _ "" = ""
multPoly "" _ = ""
multPoly a b = myNormPoly (juntaNoSoma (juntaAllProd (getTermos arvA) (getTermos arvB)))
    where
        arvA = paraArv (parseString a)
        arvB = paraArv (parseString b)


-- recebe uma arvore que representa um polinomio
-- retorna todos os termos do polinomio
getTermos :: Arv -> [Arv]
getTermos (NoSoma x l r) = getTermos l ++ getTermos r
getTermos a = [a]


-- recebe 2 listas de arvores que representam os termos dos dois polinomios
-- retorna a lista de todos os termos da primeira arvore multiplicados por todos os termos da segunda arvore
juntaAllProd :: [Arv] -> [Arv] -> [Arv]
juntaAllProd [] b = []
juntaAllProd (x:xs) b = juntaUmProd x b (length b) ++ juntaAllProd xs b


-- recebe uma arvore que representa um termo e uma lista de arvores que representa todos os termos de um polinomio
-- retorna o a lista com o produto do termo com todos os termos do outro polinomio
juntaUmProd :: Arv -> [Arv] -> Int -> [Arv]
juntaUmProd a la n = zipWith (NoProd '*') (take n (repeat a)) la


-- recebe uma lista arvores que representa os termos do polinomio
-- retorna uma arvore com os termos ligados por NoSoma's
juntaNoSoma :: [Arv] -> Arv
juntaNoSoma [a] = a
juntaNoSoma l = NoSoma '+' (juntaNoSoma (take n l))  (juntaNoSoma (drop n l))
    where n = length l `div` 2





--------------------------------------------------------- DERIVADA ---------------------------------------------------------

-- recebe um polinomio e a variavel a partir do qual se vai fazer a derivada
-- retorna o polinomio derivado
derivPoly :: String -> String -> String
derivPoly a s = myNormPoly (myDerivPoly (paraArv (parseString (normPoly a))) s)


-- recebe uma arvore que representa um polinomio e uma string que representa a variavel a partir do qual se vai fazer a derivada
-- retorna o polinomio derivado
myDerivPoly :: Arv -> String -> Arv
myDerivPoly a s = juntaNoSoma (myDerivPolyAux deriv la s)
    where
        la = prepForDeriv a
        deriv = temVariavel la s


-- recebe uma arvore que representa o polinomio
-- retorna todos os termos do polinomio numa lista de arvores
prepForDeriv :: Arv -> [Arv]
prepForDeriv (NoSoma x l r) = prepForDeriv l ++ prepForDeriv r
prepForDeriv a = [a]


-- recebe uma lista de bools e outra de termos e a variavel a ser derivada
---- se o indice i da lista de bools for true, e feita a derivada do termo no indice i
---- senao apenas adiciona 0, porque significa que nao existe aquela variavel naquele termo
-- retorna uma lista de termos derivados
myDerivPolyAux :: [Bool] -> [Arv] -> String -> [Arv]
myDerivPolyAux [] [] _ = []
myDerivPolyAux (False:xs) (y:ys) s = NoNum 0 : myDerivPolyAux xs ys s
myDerivPolyAux (True:xs) (y:ys) s = getArvDeriv y s : myDerivPolyAux xs ys s


-- recebe uma arvore que representa um termo e a variavel a partir do qual se vai fazer a derivada
-- retorna o termo derivado
getArvDeriv :: Arv -> String -> Arv
getArvDeriv (NoPoten x (NoVar v) (NoNum e)) s
    | v == s = NoProd '*' (NoNum e) (NoPoten '^' (NoVar v) (NoNum (e-1)))
    | otherwise = NoPoten x (NoVar v) (NoNum e)

getArvDeriv (NoProd x l r) s = NoProd '*' (getArvDeriv l s) (getArvDeriv r s)
getArvDeriv a _ = a


-- recebe uma lista de termos e a variavel a ser derivada
-- retorna uma lista de bools com True se o termo contiver a variavel, e False se nao tiver
temVariavel :: [Arv] -> String -> [Bool]
temVariavel [] s = []
temVariavel (x:xs) s = [temVariavelAux x s] ++ temVariavel xs s


-- funcao auxiliar que percorre o termo a procura da variavel a ser derivada
temVariavelAux :: Arv -> String -> Bool
temVariavelAux (NoPoten x (NoVar v) (NoNum e)) s
    | v == s = True
    | otherwise = False

temVariavelAux (NoProd x l r) s = temVariavelAux l s || temVariavelAux r s
temVariavelAux a _ = False