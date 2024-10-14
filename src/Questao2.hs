module Questao2 (myFloor, myCeil) where
-- Questão 2

-- Encontrar floor e ceil de um número x dentro de um array a. O número x pode não estar no array a. O floor(x) é o número do 
-- array a que é menor que x e que mais se aproxima de x (pode existir mais de um número menor que x, o floor é o maior deles). 
-- Dualmente, o ceil(x)  é o número do array a que é maior que x e que mais se aproxima de x (pode existir mais de um número maior 
-- do que x, o ceil é o menor deles).

-- myFloor
myFloor x [y] | x == y = y
             | otherwise = error "Não contém floor"
myFloor x [] = error "Não contém floor"
myFloor x (y:ys) = auxFloor x (y:ys) menor
    where
        menor = encontraMenor ys y
auxFloor x (y:ys) z | x == z = z
                    | (y <= x) && (y >= z) && length (ys) > 1 = auxFloor x ys y
                    | (y <= x) && (y >= z) && length (ys) == 1 = auxFloor x ys y
                    | length (ys) >= 1 = auxFloor x  ys z
                    | z > x = error "O Floor do número fornecido não está nesse array"
                    | otherwise = z

encontraMenor [] z = z
encontraMenor (y:ys) z | (y < z) = encontraMenor ys y
                       | (ys /= []) = encontraMenor ys z
                       | otherwise = z

-- myCeil
myCeil x [y] | x == y = y
             | otherwise = error "Não contém ceil"
myCeil x [] = error "Não contém ceil"
myCeil x (y:ys) = auxCeil x (y:ys) maior
    where
        maior = encontraMaior ys y

encontraMaior [] z = z
encontraMaior (y:ys) z | (y > z) = encontraMaior ys y
                       | (ys /= []) = encontraMaior ys z
                       | otherwise = z

auxCeil x (y:ys) z  | x == z = z
                    | (y >= x) && (y <= z) && length (ys) > 1 = auxCeil x ys y
                    | (y >= x) && (y <= z) && length (ys) == 1 = auxCeil x ys y
                    | length (ys) >= 1 = auxCeil x  ys z
                    | z < x = error "O Ceil do número fornecido não está nesse array"
                    | otherwise = z