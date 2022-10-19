# PFL_TP1_G12_02
First Project for the Course Programação Funcional e Lógica (PFL) at Faculdade de Engenharia do Porto (FEUP) 



- Uma breve descrição da estratégia de implementação de cada funcionalidade;

- Exemplos de utilização que permitam testar todas as funcionalidades do programa


## Representação dos Polinomios

### Estrutura
Neste primeiro trabalho prático de PFL decidimos usar uma árvore binária para representar os polinómios.
Nesta árvore temos 6 nós que representam os valores que podem existir num polinómio:
- **Vazia** : nó vazio, representa a string vazia;
- **NoSoma** : nó soma, representa a soma entre dois termos do polinómio;
- **NoProd** : nó produto, representa o produto entre dois elementos (quer sejam coeficientes ou variáveis) dentro de um termo;
- **NoPoten** : nó potência, representa uma variável e a sua respetiva potência;
- **NoVar** : nó variável, representa uma variável;
- **NoNum** : nó número, representa qualquer valor numérico no polinómio, seja coeficiente ou a potência de uma variável.

Dividimos sempre a estrutura em dois, na tentativa de a tornar o mais equilibrada o possível no aspeto de divisão de termos.

A seguinte imagem mostra um exemplo da utilização da árvore binária, para o polinómio "3*x^2 + 7 *x + 1":

![Arv Binaria](./pathToImage)

**Legenda:**
- +: NoSoma
- *: NoProd
- ^: NoPoten
- Qualquer caracter: NoVar
- Qualquer valor numérico: NoNum


### Justificação
A representação do polinómio com recurso a uma árvore binária pareceu-nos substancialmente mais simples no sentido em que as separações de termos e dentro dos termos ficariam mais claras e fáceis de atravessar.

Tal como foi mostrado na figura 1, os termos são separados por *NoSoma*'s, pelo que torna mais simples obter os termos. 

Procurando recursivamente por *NoSoma*'s, quando já não for encontrado um *NoSoma* significa que chegamos a um dos termos do polinómio.

Os coeficientes e variáveis, quando dentro de um termo são claramente diferenciáveis pelos *NoProd*'s.
Procurando por *NoProd*'s, quando já não for encontrado um *NoProd* implica 1 de duas possibilidades:
- Encontra NoNum: que é um valor numérico e representa um coeficiente;
- Encontra NoPoten: que é uma variável e a sua respetiva potência.

A maior **vantagem** seria a operação de derivar, já que se tornou tão simples ao ponto de ser feita em apenas 1 função. Bastava trocar um *NoPoten* por um *NoProd* que multiplicava o expoente pela variavel elevada ao (expoente-1).

A maior **desvantagem** seria a normalização do polinómio, já que obrigou à utilização de várias funções auxiliares e algumas um pouco rebuscadas.


## Funcionalidades
