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
    putStr "Normalização de '1*50*100 + 100 + 2': " >> putStr (normPoly "1*50*100 + 100 + 2") >> putStr "\n\n";
    putStr "Normalização de '2*x^2 + 5*x + 2*x': " >> putStr (normPoly "2*x^2 + 5*x + 2*x") >> putStr "\n\n";
    putStr "Normalização de '2*x*5*x^6 + x^7*2': " >> putStr (normPoly "2*x*5*x^6 + x^7*2") >> putStr "\n\n";
    putStr "Normalização de 'x^2*x^3*y + y*y': " >> putStr (normPoly "x^2*x^3*y + y*y") >> putStr "\n\n";
    putStr "Normalização de 'x^20*z^30*x^12': " >> putStr (normPoly "x^20*z^30*x^12") >> putStr "\n\n";
    testes
}

testesSoma :: IO ()
testesSoma = do{
    putStr "Soma de '1*50*100 + 100' e '-500*10 + 2': " >> putStr (sumPoly "1*50*100 + 100" "-500*10 + 2") >> putStr "\n\n";
    putStr "Soma de '2*x^2 + 5*x' e '-x^2 + 2*x': " >> putStr (sumPoly "2*x^2 + 5*x"  "-x^2 + 2*x") >> putStr "\n\n";
    putStr "Soma de '2*x*5*x^6' e 'x^7*2': " >> putStr (sumPoly "2*x*5*x^6" "x^7*2") >> putStr "\n\n";
    putStr "Soma de 'x^2*x^3*y' e 'y*y': " >> putStr (sumPoly "x^2*x^3*y" "y*y") >> putStr "\n\n";
    putStr "Soma de 'x^20*z^30*x^12' e 'x^20*z^30*x^12': " >> putStr (sumPoly "x^20*z^30*x^12" "x^20*z^30*x^12") >> putStr "\n\n";
    testes
}

testesProd :: IO ()
testesProd = do{
    putStr "Produto de '1*50*100' e '2': " >> putStr (multPoly "1*50*100" "2") >> putStr "\n\n";
    putStr "Produto de 'x + 2' e 'x - 2': " >> putStr (multPoly "x + 2" "x - 2") >> putStr "\n\n";
    putStr "Produto de '2*x^2*5' e '5*y^2*2': " >> putStr (multPoly "2*x^2*5" "5*y^2*2") >> putStr "\n\n";
    putStr "Produto de 'x^2 + y^2' e 'z^2 + x^2': " >> putStr (multPoly "x^2 + y^2" "z^2 + x^2") >> putStr "\n\n";
    putStr "Produto de 'x^20*z^30' e 'z^90*x^12': " >> putStr (multPoly "x^20*z^30" "z^90*x^12") >> putStr "\n\n";
    testes  
}

testesDeriv :: IO ()
testesDeriv = do{
    putStr "Derivada de '5 + 10*2 + 25' em ordem a 'x': " >> putStr (derivPoly "5 + 10*2 + 25" "x") >> putStr "\n\n";
    putStr "Derivada de '2*x^2 + 5*x + 2*x^3' em ordem a 'x': " >> putStr (derivPoly "2*x^2 + 5*x + 2*x^3" "x") >> putStr "\n\n";
    putStr "Derivada de '2*x^2*y^3*z^5*5' em ordem a 'y': " >> putStr (derivPoly "2*x^2*y^3*z^5*5" "y") >> putStr "\n\n";
    putStr "Derivada de 'x^2*x^3*y + y*y' em ordem a 'y': " >> putStr (derivPoly "x^2*x^3*y + y*y" "y") >> putStr "\n\n";
    putStr "Derivada de 'x^20*z^30*x^12 - z^200 + 50' em ordem a 'z': " >> putStr (derivPoly "x^20*z^30*x^12 - z^200 + 50" "z") >> putStr "\n\n";  
    testes
}