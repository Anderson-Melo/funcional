module Questao1 (escolheTupla) where

-- Dado um array ordenado e um número x, encontre um par (a,b) de números pertencentes 
-- ao array tal que (a + b) se aproxime o máximo possível de x.

-- Input: arr[] = {10, 22, 28, 29, 30, 40}, x = 54
-- Output: 22 and 30
-- Input: arr[] = {1, 3, 4, 7, 10}, x = 15
-- Output: 4 and 10

escolheTupla x [] = (0, 0)
escolheTupla _ [y] = error "Lista só possui 1 elemento, impossível criar uma par"
escolheTupla x arr = auxVerificaTuplas x tuplasSelecionadas menorTupla
    where
        tuplasSelecionadas = (encontraTuplas arr)
        menorTupla = menorSoma tuplasSelecionadas

auxVerificaTuplas _ [] (a, b) = (a, b)
auxVerificaTuplas x arr (a, b) 
    | abs (somaAtual - x) < abs (melhorSoma - x) = auxVerificaTuplas x (tail arr) (head arr)
    | otherwise = auxVerificaTuplas x (tail arr) (a, b)
  where
    somaAtual = fst (head arr) + snd (head arr)
    melhorSoma = a + b

encontraTuplas [] = []
encontraTuplas (x:xs) = [(x, y) | y <- xs] ++ encontraTuplas xs

menorSoma [] = error "A lista esta vazia"
menorSoma (x:xs) = foldl comparaTuplas x xs
  where
    comparaTuplas tupla1 tupla2
      | soma tupla1 <= soma tupla2 = tupla1
      | otherwise                   = tupla2
    soma (a, b) = a + b