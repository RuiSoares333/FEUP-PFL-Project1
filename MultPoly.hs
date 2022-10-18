module MultPoly where
import GrowTree
import MinceString
import NormExp



multPoly :: Arv a -> Arv a -> Arv a
multPoly a b = juntaNoSoma (juntaAllProd (getTermos a) (getTermos b))



getTermos :: Arv a -> [Arv a]
getTermos (NoSoma x l r) = getTermos l ++ getTermos r
getTermos a = [a]



juntaAllProd :: [Arv a] -> [Arv a] -> [Arv a]
juntaAllProd [] b = []
juntaAllProd (x:xs) b = juntaUmProd x b (length b) ++ juntaAllProd xs b



juntaUmProd :: Arv a -> [Arv a] -> Int -> [Arv a]
juntaUmProd a la n = zipWith (NoProd '*') (take n (repeat a)) la



juntaNoSoma :: [Arv a] -> Arv a
juntaNoSoma [a] = a 
juntaNoSoma l = NoSoma '+' (juntaNoSoma (take n l))  (juntaNoSoma (drop n l)) 
    where n = length l `div` 2