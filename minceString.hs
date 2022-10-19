module MinceString where


-- Parse da string : Feito
-- 1. Receber a string 
-- 2. Partir os "+"
-- 3. Partir os "*" - problema quando tem 1, nao tem variavel x
-- 4. Partir os "^" - problema quando tem 0 pq nao tem "^", quando tem 1 porque nao existe o "1"



-- recebe um polinomio em string ex.: "5*x^2"
-- retorna uma estrutura do seguinte tipo ex.: [[["5"],["x","2"]]]
parseString :: String -> [[[String]]]
parseString [] = [[[]]]
parseString s = tiraVezes (tiraMais (simpMenos (removeEspaco s)))


-- remove todos os espacos para simplificar a string de input
-- recebe a string do polinomio do input
-- retorna a mesma string mas sem espacos
removeEspaco :: String -> String
removeEspaco s = [x | x <- s, x/=' ']


-- adicionar o char '+' antes de todos os chars '-' porque a separacao dos termos e feita a partir dos '+'
-- recebe a string do polinomio do input
-- retorna a mesma string mas com caracteres de '+' antes dos caracteres de '-'
simpMenos :: String -> String
simpMenos [] = ""
simpMenos (x:xs)
        | x == '^' = x : head xs : simpMenos (tail xs)
        | x == '-' = "+-" ++ simpMenos xs
        | otherwise =  x : simpMenos xs


-- separa os termos pelo char '+' que transforma um input do tipo: "x+5*y+7*z^2" -> ["x", "5*y", "7*z^2"]
-- recebe a string de input com o polinomio
-- retorna uma lista de strings com os termos do polinomio
tiraMais :: String -> [String]
tiraMais = split '+'


-- separa os componentes de um termo que transforma um input do tipo: ["x", "5*y", "7*z^2"] -> [[["x"]],[["5"],["y"]],[["7"],["z","2"]]]
-- recebe uma lista de strings com os termos do polinomio
-- retorna uma estrutura com os termos
---- dentro de cada termo tem [[coeficiente, [[variavel, potencia], ...] ], ...]
tiraVezes :: [String] -> [[[String]]]
tiraVezes = map (tiraPoten . split '*')


-- separa os compontentes de uma variavel elevada a uma dada potencia. O que corresponde a uma operacao do tipo ["z^2"] -> [["z","2"]]
-- recebe uma lista de strings com os termos do polinomio
-- retorna uma lista de cada elemento da lista separado pelo caracter '^'
tiraPoten :: [String] -> [[String]]
tiraPoten  [] = []
tiraPoten (x:xs) = [split '^' x] ++ tiraPoten xs


-- separa todos os elementos de uma string numa lista de strings, em que o caracter de separacao pode ser escolhido
-- recebe um caracter e uma string
-- retorna uma lista com strings provenientes da string original que foi separada pelo caracter
split :: Char -> String -> [String]
split _ "" = []
split c s = firstWord : split c rest
    where firstWord = takeWhile (/=c) s
          rest = drop (length firstWord + 1) s
