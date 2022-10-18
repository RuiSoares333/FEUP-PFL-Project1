module VarsExistentes where
import GrowTree ( Arv(NoProd, NoSoma, NoPoten, NoVar, NoNum) )
import Data.List
import Utils



-- primeiro pegar em todas as variaveis que existem na arvore
-- depois juntar em elementos diferentes tipo [[x^2, 5*x^2, 7*x^2], [y^2, 10*y^2]]
-- depois fazer a simplificação da soma [[13*x^2], [11*y^2]]
-- x^2 + y^2 + 5*x^2


-- retorna as variaveis existentes em cada termo do polinomio
varEx :: Arv a -> [[String]]
varEx a =  remDup [sort x | x <- varExSoma a]


-- separa as variaveis por termo
varExSoma :: Arv a-> [[String]]
varExSoma (NoSoma x l r) = varExSoma l ++ varExSoma r

varExSoma a = [zipWith (++) vars (zipWith (++) (replicate n "^") exp)]
    where
        vars = remDup2 (varExProd a)
        n = length vars
        exp = [show x | x <- somaExp vars a (populateList n)]

-- retorna as variaveis dentro de um termo
varExProd :: Arv a-> [String]
varExProd (NoProd x l r) = varExProd l ++ varExProd r
varExProd (NoPoten x (NoVar l) (NoNum r)) = [l]
varExProd a = []



-- acumula os expoentes de um termo numa lista de ints que corresponde a sua posicao/variavel
-- [x, y], arvore com tudo -> procura pela arvore pelos numeros que correspondem
somaExp :: [String] -> Arv a->[Int] -> [Int]
somaExp [] _ l = l
somaExp (x:xs) t l = somaExp xs t (insertInIndex id l coef)
    where
        coef = sum (findNoPoten t x)
        id = myFindIndex x (remDup2 (varExProd t)) 0


-- encontra os todos os expoentes de uma dada variavel dentro de um termo 
findNoPoten :: Arv a-> String -> [Int]
findNoPoten (NoPoten x (NoVar l) (NoNum r)) s
    | l==s = [r]
    | otherwise = [0]

findNoPoten (NoProd x l r) s = findNoPoten l s ++ findNoPoten r s
findNoPoten a s= [0]

remDup2 :: [String] -> [String]
remDup2 l = map head (group (sort l))