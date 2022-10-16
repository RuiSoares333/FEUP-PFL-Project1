module MinceString where


-- Parse da string : Feito
-- 1. Receber a string 
-- 2. Partir os "+"
-- 3. Partir os "*" - problema quando tem 1, nao tem variavel x
-- 4. Partir os "^" - problema quando tem 0 pq nao tem "^", quando tem 1 porque nao existe o "1"


---- Chamada de todas as funções que transforma um input do tipo: "5*x^2" -> [[["5"],["x","2"]]]
trataString :: String -> [[[String]]]
trataString [] = [[[]]]
trataString s = tiraVezes (tiraMais (simpMenos (removeEspaco s)))


---- Remoção de todas as instancias de espaços para efeitos de simplificação da String de Input
removeEspaco :: String -> String
removeEspaco s = [x | x <- s, x/=' ']


---- Adicionar o char '+' antes de todos os chars '-' porque a separação dos termos é feita a partir dos '+'
simpMenos :: String -> String
simpMenos [] = ""
simpMenos (x:xs)
        | x == '^' = x : head xs : simpMenos (tail xs)
        | x == '-' = "+-" ++ simpMenos xs
        | otherwise =  x : simpMenos xs


---- Separar os termos pelo char '+' que transforma um input do tipo: "x+5*y+7*z^2" -> ["x", "5*y", "7*z^2"]
tiraMais :: String -> [String]
tiraMais s =  split '+' s


---- Separa os componentes de um termo que transforma um input do tipo: ["x", "5*y", "7*z^2"] -> [[["x"]],[["5"],["y"]],[["7"],["z^2"]]]
tiraVezes :: [String] -> [[[String]]]
tiraVezes [] = []
tiraVezes (x:xs) =  [tiraPotencia (split '*' x)] ++ tiraVezes xs


---- Separa todos os elementos de uma string numa lista de strings, em que o caracter de separacao pode ser escolhido
split :: Char -> String -> [String]
split _ "" = []
split c s = firstWord : (split c rest)
    where firstWord = takeWhile (/=c) s
          rest = drop (length firstWord + 1) s


---- Separa os compontentes de uma variavel elevada a uma dada potencia. O que corresponde a uma operacao do tipo ["z^2"] -> [["z","2"]]
tiraPotencia :: [String] -> [[String]]
tiraPotencia  [] = []
tiraPotencia (x:xs) = [split '^' x] ++ tiraPotencia xs