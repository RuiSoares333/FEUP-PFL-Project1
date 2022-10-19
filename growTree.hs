module GrowTree where

-- estrutura escolhida: uma arvore binaria
-- explicada no ficheiro README
data Arv a = Vazia
            | NoSoma Char (Arv a) (Arv a) -- soma entre dois termos
            | NoProd Char (Arv a) (Arv a) -- produto entre cada elemento de um termo
            | NoPoten Char (Arv a) (Arv a) --  potencia de uma variavel
            | NoVar String                 -- variavel em string
            | NoNum Int deriving Show      -- coeficiente



-- Construir a arvore : Feito
-- 1. Separar o polinomio pela metade   :  [[["5"],["x","2"]],[["-10"],["y","3"],["x","2"]]] -> [["5"],["x","2"]]  &  [["-10"],["y","3"],["x","2"]]
        -- 1.5 separa-los na arvore com '+'

-- 2. Separar os termos pela metade   :   [["5"],["x","2"]] ->  ["5"]   &   ["x","2"]
        -- 2.5 separa-los com '*'

-- 3. Separar as variaveis das potencias : ["x","2"] ->  "x"   &   "2"
        -- 3.5 separa-los com  '^'


-- transforma um input do tipo [[["7"],["x","2"]]] -em-> NoProd '*' (NoNum 7) (NoPoten '^' (NoVar "x") (NoNum 2))
paraArv :: [[[String]]] -> Arv a
paraArv [[[]]] = Vazia
paraArv lCoef
        | (head lCoef) == [] = paraArv (tail lCoef)
        | s == 0 = separaCoef (head lCoef)
        | otherwise = NoSoma '+' (paraArv (take s lCoef)) (paraArv (drop s lCoef))
        where s = length lCoef `div` 2


-- separa as variaveis dentro de um termo com NoProd's
---- 7*x^2 = NoProd '*' (NoNum 7) (NoPoten '^' (NoVar "x") (NoNum 2)) 
separaCoef :: [[String]] -> Arv a
separaCoef [[]] = Vazia
separaCoef coef
        | s == 0 = separaFolha (head coef)
        | otherwise = NoProd '*' (separaCoef (take s coef)) (separaCoef (drop s coef))
        where s = length coef `div` 2


-- decide se uma folha da arvore e um numero ou uma variavel
---- se for numero cria um NoNum
---- senao cria NoPoten
separaFolha :: [String] -> Arv a
separaFolha [] = Vazia
separaFolha e
        | v = NoProd '*' (NoNum (-1)) (separaFolha (tail (head e) : tail e))
        | s == 1 = numOuVar (head e)
        | otherwise = NoPoten '^' (NoVar (head e)) (NoNum (read (head (tail e)) :: Int))
        where
                s = length e
                v = head (head e) == '-' && not(checkIfDigit(tail (head e)))


-- recebe uma string
---- se for numero retorna um NoNum
---- senao retorna NoPoten
numOuVar :: String -> Arv a
numOuVar e
        | v = NoNum (read e :: Int)
        | otherwise = NoPoten '^'  (NoVar e) (NoNum 1)
        where v = checkIfDigit e


-- lista de todos os digitos e o caracter '-'
digitos :: [Char]
digitos = "0123456789-"


-- recebe uma string
---- retorna True se for digito ou '-'
---- senao retorna False
checkIfDigit :: String -> Bool
checkIfDigit [] = True
checkIfDigit (x:xs)
        | v = checkIfDigit xs
        | otherwise = False
        where v = myIsDigit x digitos


-- recebe um caracter e uma lista com todos os digitos
---- retorna True se o caracter for um dos digitos
---- senao retorna False
myIsDigit :: Char -> String -> Bool
myIsDigit _ []= False
myIsDigit n (x:xs)
  | x == n = True
  | otherwise = myIsDigit n xs
