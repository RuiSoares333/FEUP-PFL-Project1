removeChar :: Char -> String -> String {-usar para remover os espaços-}
removeChar _ [] = []
removeChar ch (c:cs)
    | c == ch   = removeChar ch cs
    | otherwise = c:(removeChar ch cs)


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
entrada s = map prod (espacos s)

espacos :: String -> [String]
espacos s = split ' ' s

prod :: String -> [[String]]
prod s = map poten (split '*' s)

poten :: String -> [String]
poten s = split '^' s
