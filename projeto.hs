data Variavel = RegVar Char Int deriving Show-- [x, 2] = x^2

data Expressao = RegEx Int Variavel deriving Show-- [5, [x, 2]] = 5*x^2
data Arv a = Vazia 
            | NoChar Char (Arv a) (Arv a)
            | NoInt Int (Arv a) (Arv a)

split :: Char -> String -> [String] {-várias chamadas para sucessivamente partir a expressão-}
split _ "" = []
split c s = firstWord : (split c rest)
    where firstWord = takeWhile (/=c) s
          rest = drop (length firstWord + 1) s




{- 1. Receber a string 
   2. Partir os "+"
   3. Partir os "*" - problema quando tem 1, nao tem variavel x
   4. Partir os "^" - problema quando tem 0 pq nao tem "^", quando tem 1 pq nao existe o "1"-}

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
