data Variavel = RegVar Char Int deriving Show-- [x, 2] = x^2

data Expressao = RegEx Int Variavel deriving Show-- [5, [x, 2]] = 5*x^2

data Arv a = Vazia 
            | NoSoma Char (Arv a) (Arv a)
            | NoProd Char (Arv a) (Arv a)
            | NoPoten Char (Arv a) (Arv a)
            | NoVar Char 
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

-- Construir a arvore
-- 1. Separar o polinómio pela metade   :  [[["5"],["x","2"]],[["-10"],["y","3"],["x","2"]]] -> [["5"],["x","2"]]  &  [["-10"],["y","3"],["x","2"]]
        -- 1.5 separa-los na arvore com '+'

-- 2. Separar os coeficientes pela metade   :   [["5"],["x","2"]] ->  ["5"]   &   ["x","2"]
        -- 2.5 separa-los com '*'

-- 3. Separar as variaveis das potencias : ["x","2"] ->  "x"   &   "2"
        -- 3.5 separa-los com  '^'


separaPoli :: [[[String]]] -> Arv a
separaPoli lCoef
        | s==1 = separaCoef (head lCoef)
        | otherwise = NoSoma '+' (separaPoli (take s lCoef)) (separaPoli (drop s lCoef))
        where s = (length lCoef) `div` 2

separaCoef :: [[String]] -> Arv a
separaCoef [_] = Vazia
separaCoef coef =
        NoProd '*' (separaCoef (take s coef)) (separaCoef (drop s coef))
        where s = (length coef) `div` 2

separaPoten :: [String] -> Arv a
separaPoten var =
        NoPoten '^' (NoVar head var) (NoNum (read (tail var) :: Int))

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

tuplify2 :: [a] -> (a, a)
tuplify2 [x, y] = (x, y)

tiraVezes :: [String] -> [[[String]]]
tiraVezes [] = []
tiraVezes (x:xs) =  [tiraPotencia (split '*' x)] ++ tiraVezes xs

tiraPotencia :: [String] -> [[String]]
tiraPotencia  [] = []
tiraPotencia (x:xs) = [split '^' x] ++ tiraPotencia xs
