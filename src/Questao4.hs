module Questao4 (MyQueue(..), myQueueAdd, myQueueRemove, myQueuePoll, myQueueElement, myQueuePeek) where
-- Implementar uma Fila e seus algoritmos em Haskell. Use a lista de Haskell como estrutura sobrejacente e operações que não sejam acesso pelo índice.

data MyQueue q = Nil | MyQueue [q] deriving (Show, Eq)

myQueueAdd e (MyQueue q) = MyQueue (e:q)

myQueueRemove (MyQueue []) = error "Elemento não existe"
myQueueRemove (MyQueue (x:xs)) = (x, MyQueue xs)

myQueuePoll (MyQueue []) = (Nil, Nil)
myQueuePoll (MyQueue (x:xs)) =  (x, MyQueue xs)

myQueueElement (MyQueue []) = error "Não existe elementos na fila"
myQueueElement (MyQueue (x:xs)) = x

myQueuePeek (MyQueue []) = Nil
myQueuePeek (MyQueue (x:xs)) = x