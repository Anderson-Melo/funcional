-- Dado um array ordenado e um número x, encontre um par (a,b) de números pertencentes 
-- ao array tal que (a + b) se aproxime o máximo possível de x.

-- Input: arr[] = {10, 22, 28, 29, 30, 40}, x = 54
-- Output: 22 and 30
-- Input: arr[] = {1, 3, 4, 7, 10}, x = 15
-- Output: 4 and 10

escolheTupla x arr = auxVerificaTuplas x (encontraTuplas arr) (0, 0)

auxVerificaTuplas _ [x] _ = x 
auxVerificaTuplas x arr (a, b) | tuplaAtualSoma == x = (head arr)
                               | abs (x - tuplaAtualSoma) < abs (x - ultimaTuplaSoma) && length (arr) > 1 = auxVerificaTuplas x (tail arr) (head arr)
                               | abs (x - tuplaAtualSoma) > abs (x - ultimaTuplaSoma) && length (arr) > 1 = auxVerificaTuplas x (tail arr) (a, b)
                               | length arr == 1 && abs (x - tuplaAtualSoma) > abs (x - ultimaTuplaSoma)= auxVerificaTuplas x [head arr] (a, b)
                               | otherwise = (a, b)
    where
        ultimaTuplaSoma = abs (a + b)
        tuplaAtualSoma = abs ((fst (head arr)) + (snd (head arr)))

encontraTuplas arr =  [(a, b) | a <- arr, b <- arr, a /= b]