import MinceString

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


---- Chama todas as funcoes e transforma um input do tipo [[["7"],["x","2"]]] -> NoProd '*' (NoNum 7) (NoPoten '^' (NoVar "x") (NoNum 2))
---- tambem separa os termos de uma expressao com NoSoma's
separaPoli :: [[[String]]] -> Arv a
separaPoli [[[]]] = Vazia
separaPoli lCoef
        | head lCoef == [] = separaPoli (tail lCoef)
        | s == 0 = separaCoef (head lCoef)
        | otherwise = NoSoma '+' (separaPoli (take s lCoef)) (separaPoli (drop s lCoef))
        where s = (length lCoef) `div` 2


-- separa as variaveis dentro de um termo com NoProd's
separaCoef :: [[String]] -> Arv a
separaCoef [[]] = Vazia
separaCoef coef
        | s == 0 = separaFolha (head coef)
        | otherwise = NoProd '*' (separaCoef (take s coef)) (separaCoef (drop s coef))
        where s = (length coef) `div` 2



-- decide se uma folha da arvore e um numero ou uma variavel
separaFolha :: [String] -> Arv a
separaFolha [] = Vazia
separaFolha e
        | v = NoProd '*' (NoNum (-1)) (separaFolha ((tail (head e)) : (tail e)))
        | s == 1 = numOuVar (head e)
        | otherwise = NoPoten '^' (NoVar (head e)) (NoNum (read (head (tail e)) :: Int))
        where 
                s = length e
                v = head (head e) == '-' && (not)(checkIfDigit(tail (head e)))


-- verifica se uma string e um numero ou nao
numOuVar :: String -> Arv a
numOuVar e
        | v = NoNum (read e :: Int)
        | otherwise = NoPoten '^'  (NoVar e) (NoNum 1)
        where v = checkIfDigit e



-- verifica se uma string e um digito ou nao
checkIfDigit :: String -> Bool
checkIfDigit [] = True
checkIfDigit (x:xs)
        | v = checkIfDigit xs
        | otherwise = False
        where v = myIsDigit x

-- funcao auxiliar para vetrificar se um char e um digito
myIsDigit :: Char -> Bool
myIsDigit '0' = True
myIsDigit '1' = True
myIsDigit '2' = True
myIsDigit '3' = True
myIsDigit '4' = True
myIsDigit '5' = True
myIsDigit '6' = True
myIsDigit '7' = True
myIsDigit '8' = True
myIsDigit '9' = True
myIsDigit '-' = True
myIsDigit _ = False