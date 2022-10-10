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

hakuna :: String -> [String]{-[(a, a, a)]-}
hakuna s = facildizer (matata [ x | x <- split '+' (removeChar ' ' s)])
{-
hakuna1 :: String -> [String]
hakuna1 s = [ x | x <- split '*' s]-}

matata :: [String] -> (a, a)
matata [] = []
matata (x:xs) =  split '*' x ++ matata xs

facildizer :: [(a, a)] -> [(a, a, a)]
facildizer [] = []
facildizer t = split '^' snd t ++ facildizer xs