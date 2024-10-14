-- Implementar uma Fila e seus algoritmos em Haskell. Use a lista de Haskell como estrutura sobrejacente e operações que não sejam acesso pelo índice.

data MyQueue q = Nil | MyQueue [q] deriving (Show, Reverse)

myQueueAdd e (MyQueue q) = MyQueue (e:q)

myQueueRemove (MyQueue []) = error "Elemento não existe"
myQueueRemove (MyQueue q) = reverse (tail (reverse (MyQueue q))) -- tem que retornar o elemento

myQueuePoll (MyQueue []) = Nil
myQueuePoll (MyQueue q) = reverse (tail (reverse (MyQueue q))) -- tem que retornar o elemento

myQueueElement (MyQueue []) = error "Não existe elementos na fila"
myQueueElement (MyQueue (x:xs)) = x

myQueuePeek (MyQueue []) = Nil
myQueuePeek (MyQueue (x:xs)) = x