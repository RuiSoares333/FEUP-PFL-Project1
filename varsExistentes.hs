module VarsExistentes where

import GrowTree
import Data.List


-- primeiro pegar em todas as variaveis que existem na arvore
-- depois juntar em elementos diferentes tipo [[x^2, 5*x^2, 7*x^2], [y^2, 10*y^2]]
-- depois fazer a simplificação da soma [[13*x^2], [11*y^2]]
-- x^2 + y^2 + 5*x^2

varEx :: Arv a -> [[String]]
varEx a =  remDup [sort x | x <- varExSoma a, x /= []]

remDup :: [[String]] -> [[String]]
remDup l = map head (group (sort l))

varExSoma :: Arv a -> [[String]]
varExSoma (NoSoma x l r) = (varExSoma l) ++ (varExSoma r)
varExSoma a = [varExProd a]

-- 5*6 = Vazia?
-- 5*x => x1
-- x*5 => x1
-- x^2 => x2
varExProd :: Arv a -> [String]
varExProd (NoNum a) = []
varExProd (NoProd x (NoNum y) (NoNum b)) = []
varExProd (NoPoten x (NoVar l) (NoNum r)) = [l ++ "^" ++ (show r)]

varExProd (NoProd x (NoNum a) (NoPoten b (NoVar l) (NoNum r))) = [l ++ "^" ++ (show r)]
varExProd (NoProd x (NoPoten a (NoVar l) (NoNum r)) (NoNum b)) = [l ++ "^" ++ (show r)]

varExProd (NoProd x (NoNum a) (NoProd b l r)) = varExProd (NoProd b l r)
varExProd (NoProd x (NoProd a l r) (NoNum b)) = varExProd (NoProd a l r)
varExProd (NoProd x l r) = (varExProd l) ++ (varExProd r)