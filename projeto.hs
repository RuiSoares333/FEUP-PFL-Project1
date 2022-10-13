import Data.Char (isDigit)

data Variavel = RegVar Char Int deriving Show-- [x, 2] = x^2

data Expressao = RegEx Int Variavel deriving Show-- [5, [x, 2]] = 5*x^2

data Arv a = Vazia 
            | NoSoma Char (Arv a) (Arv a)
            | NoProd Char (Arv a) (Arv a)
            | NoPoten Char (Arv a) (Arv a)
            | NoVar String 
            | NoNum Int deriving Show

split :: Char -> String -> [String] {-várias chamadas para sucessivamente partir a expressão-}
split _ "" = []
split c s = firstWord : (split c rest)
    where firstWord = takeWhile (/=c) s
          rest = drop (length firstWord + 1) s



-- Desconstruir a String : Feito
{- 1. Receber a string 
   2. Partir os "+"
   3. Partir os "*" - problema quando tem 1, nao tem variavel x
   4. Partir os "^" - problema quando tem 0 pq nao tem "^", quando tem 1 pq nao existe o "1"-}

-- Construir a arvore : Feito
-- 1. Separar o polinómio pela metade   :  [[["5"],["x","2"]],[["-10"],["y","3"],["x","2"]]] -> [["5"],["x","2"]]  &  [["-10"],["y","3"],["x","2"]]
        -- 1.5 separa-los na arvore com '+'

-- 2. Separar os coeficientes pela metade   :   [["5"],["x","2"]] ->  ["5"]   &   ["x","2"]
        -- 2.5 separa-los com '*'

-- 3. Separar as variaveis das potencias : ["x","2"] ->  "x"   &   "2"
        -- 3.5 separa-los com  '^'


separaPoli :: [[[String]]] -> Arv a
separaPoli lCoef
        | s == 0 = separaCoef (head lCoef)
        | otherwise = NoSoma '+' (separaPoli (take s lCoef)) (separaPoli (drop s lCoef))
        where s = (length lCoef) `div` 2

separaCoef :: [[String]] -> Arv a
separaCoef coef
        | s == 0 = separaFolha (head coef)
        | otherwise = NoProd '*' (separaCoef (take s coef)) (separaCoef (drop s coef))
        where s = (length coef) `div` 2

separaFolha :: [String] -> Arv a
separaFolha e
        | s == 1 = numOuVar (head e)
        | otherwise = NoPoten '^' (NoVar (head e)) (NoNum (read (head (tail e)) :: Int))
        where s = length e

numOuVar :: String -> Arv a
numOuVar e
        | v = NoNum (read e :: Int)
        | otherwise = NoPoten '^'  (NoVar e) (NoNum 1)
        where v = checkIfDigit e


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
myIsDigit _ = False

checkIfDigit :: String -> Bool
checkIfDigit [] = True
checkIfDigit (x:xs)
        | v = checkIfDigit xs
        | otherwise = False
        where v = isDigit x





entrada :: String -> [[[String]]]
entrada s = tiraVezes (tiraMais (simpMenos (removeEspaco s)))

removeEspaco :: String -> String
removeEspaco s = [x | x <- s, x/=' ']

simpMenos :: String -> String
simpMenos [] = ""
simpMenos (x:xs) 
        | x == '-' = "+-" ++ simpMenos xs
        | otherwise =  x : simpMenos xs

tiraMais :: String -> [String]
tiraMais s =  split '+' s

tiraVezes :: [String] -> [[[String]]]
tiraVezes [] = []
tiraVezes (x:xs) =  [tiraPotencia (split '*' x)] ++ tiraVezes xs

tiraPotencia :: [String] -> [[String]]
tiraPotencia  [] = []
tiraPotencia (x:xs) = [split '^' x] ++ tiraPotencia xs
