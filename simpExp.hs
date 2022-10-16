import GrowTree
import MinceString

-- Pegar num NoSoma -> (NoSOma x l r)
-- Ver ls e rs
-- Ver o left e right de ls e rs


normSumPoli :: Arv a -> Arv a
normsumPoli Vazia Vazia = Vazia
normsumPoli Vazia a = a
normsumPoli a Vazia = a
normSumPoli (NoSoma x l r) = normProdPoli l r



-- primeiro pegar em todas as variaveis que existem na arvore
-- depois juntar em elementos diferentes tipo [[x^2, 5*x^2, 7*x^2], [y^2, 10*y^2]]
-- depois fazer a simplificação da soma [[13*x^2], [11*y^2]]
-- x^2 + y^2 + 5*x^2

-- 5*6 = Vazia?
-- 5*x => x
-- x*5 => x
varsExistentes :: Arv a -> [String]
varsExistentes Vazia = [""]
varsExistentes (NoProd x (NoNum y) (NoNum b)) = [""]

varsExistentes (NoProd x (NoVar a) (NoNum b)) = [a]
varsExistentes (NoProd x (NoNum a) (NoVar b)) = [b]

varsExistentes (NoProd x (NoVar a) (NoPoten b (NoVar l) (NoNum r))) = [a ++ l ++ (show r) ++ "*"]
varsExistentes (NoProd x (NoPoten a (NoVar l) (NoNum r)) (NoVar b)) = [l ++ (show r) ++ b]
varsExistentes (NoProd x (NoPoten a (NoVar la) (NoNum ra)) (NoPoten b (NoVar lb) (NoNum rb))) = [la ++ (show ra) ++ lb ++ (show rb)]

varsExistentes (NoSoma x l r) = (varsExistentes l) ++ (varsExistentes r)
varsExistentes (NoProd x (NoProd a la ra) (NoProd b lb rb)) = (varsExistentes (NoProd a la ra)) ++ (varsExistentes (NoProd b lb rb))
varsExistentes (NoProd x (NoNum a) (NoProd b l r)) = varsExistentes (NoProd b l r)
varsExistentes (NoProd x (NoProd a l r) (NoNum b)) = varsExistentes (NoProd a l r)


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
