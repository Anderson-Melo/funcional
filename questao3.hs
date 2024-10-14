-- Implementar uma pilha e seus algoritmos em Haskell. Use a lista de Haskell como estrutura sobrejacente e operações que não sejam acesso pelo índice.

data MyStack s = Nil | MyStack [s] deriving (Show)

myStackPush e (MyStack xs) = MyStack (e:xs)

myStackPop Nil = Nil
myStackPop (MyStack []) = error "Lista Vazia, impossível remover elemento"
myStackPop (MyStack (x:xs)) = (x, MyStack xs) 

myStackPeek Nil = error "Lista Nula"
myStackPeek (MyStack []) = error "Lista vazia"
myStackPeek (MyStack (x:xs)) = x 

myStackEmpty Nil = error "Pilha nula"
myStackEmpty (MyStack []) = True
myStackEmpty (MyStack _) = False

myStackSearch _ (MyStack []) = -1
myStackSearch _ Nil = -1
myStackSearch e (MyStack (x:xs)) = auxSearch e (MyStack (x:xs)) 1

auxSearch e (MyStack (x:xs)) a | e == x = a
                               | e /= x && length (xs) > 0 = auxSearch e (MyStack xs) (a+1)
                               | (a == 1) || (e /= x) = -1
                               | otherwise = a


