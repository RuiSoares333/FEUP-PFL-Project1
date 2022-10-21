module Main where

import Functions

main :: IO ()
main = 
    do{
        putStr "Escolha a operação que quer realizar:\n\n";
        putStr "1 - Utilizar as operações em polinómios\n";
        putStr "2 - Ver casos de utilização das operações em polinómios\n\n";
        putStr "0 - Sair\n\n";

        a <- getLine;
        if a == "1" then operacoes;
        else if a == "2" then testes;
        else if a == "0" then return ();
        else main;
}



--------------------------------------------------------- OPERAÇÕES ---------------------------------------------------------

operacoes :: IO ()
operacoes = 
    do {
        putStr "Escolha a operação que quer realizar:\n\n";
        putStr "1 - Normalização\n";
        putStr "2 - Soma\n";
        putStr "3 - Produto\n";
        putStr "4 - Derivada\n\n";
        putStr "0 - Voltar\n";

        a <- getLine;
        if a == "1" then operacaoNorm;
        else if a == "2" then operacaoSoma;
        else if a == "3" then operacaoProd;
        else if a == "4" then operacaoDeriv;
        else if a == "0" then main;
        else operacoes;
}

operacaoNorm :: IO ()
operacaoNorm =
    do{
        putStr "Normalização -> Introduza o polinómio\n";
        a <- getLine;
        putStr "Normalização -> Resultado: " >> putStr (normPoly a) >> putStr "\n\n";
        operacoes
    }

operacaoSoma :: IO ()
operacaoSoma =
    do{
        putStr "Soma -> Introduza o primeiro polinómio\n";
        a <- getLine;
        putStr "Soma -> Introduza o segundo polinómio\n";
        b <- getLine;
        putStr "Soma -> Resultado: " >> putStr (sumPoly a b)  >> putStr "\n\n";
        operacoes
    }

operacaoProd :: IO ()
operacaoProd =
    do{
        putStr "Produto -> Introduza o primeiro polinómio\n";
        a <- getLine;
        putStr "Produto -> Introduza o segundo polinómio\n";
        b <- getLine;
        putStr "Produto -> Resultado: " >> putStr (multPoly a b)  >> putStr "\n\n";
        operacoes
    }

operacaoDeriv :: IO ()
operacaoDeriv =
    do{
        putStr "Derivação -> Introduza o polinómio\n";
        a <- getLine;
        putStr "Derivação -> Introduza a variável à qual o polinómio vai ser derivada\n";
        b <- getLine;
        putStr "Derivação -> Resultado: " >> putStr (derivPoly a b) >> putStr "\n\n";
        operacoes
    }




--------------------------------------------------------- TESTES ---------------------------------------------------------

testes :: IO ()
testes = do {
        putStr "Escolha os exemplos que quer visualizar:\n\n";
        putStr "1 - Normalização\n";
        putStr "2 - Soma\n";
        putStr "3 - Produto\n";
        putStr "4 - Derivada\n\n";
        putStr "0 - Voltar\n";


        a <- getLine;
        if a == "1" then testesNorm;
        else if a == "2" then testesSoma;
        else if a == "3" then testesProd;
        else if a == "4" then testesDeriv;
        else if a == "0" then main;
        else operacoes;
}

testesNorm :: IO ()
testesNorm = do{
    putStr "Teste com apenas números\n";
    putStr "Teste com números e variáveis simples\n";
    putStr "Teste com numeros e variaveis intercalados tipo 2*x*4*y^2*5*z^3*2\n";
    putStr "Teste com apenas variaveis\n";
    putStr "Teste com apenas variaveis com potencias elevadas\n";
    testes
}

testesSoma :: IO ()
testesSoma = do{
    putStr "Teste com apenas números\n";
    putStr "Teste com números e variáveis simples\n";
    putStr "Teste com numeros e variaveis intercalados tipo 2*x*4*y^2*5*z^3*2\n";
    putStr "Teste com apenas variaveis\n";
    putStr "Teste com apenas variaveis com potencias elevadas\n";
    testes
}

testesProd :: IO ()
testesProd = do{
    putStr "Teste com apenas números\n";
    putStr "Teste com números e variáveis simples\n";
    putStr "Teste com numeros e variaveis intercalados tipo 2*x*4*y^2*5*z^3*2\n";
    putStr "Teste com apenas variaveis\n";
    putStr "Teste com apenas variaveis com potencias elevadas\n";  
    testes  
}

testesDeriv :: IO ()
testesDeriv = do{
    putStr "Teste com apenas números\n";
    putStr "Teste com números e variáveis simples\n";
    putStr "Teste com numeros e variaveis intercalados tipo 2*x*4*y^2*5*z^3*2\n";
    putStr "Teste com apenas variaveis\n";
    putStr "Teste com apenas variaveis com potencias elevadas\n";    
    testes
}