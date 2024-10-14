-- Um aluno é representado como uma estrutura contando matrícula, um primeiro nome, um sobrenome, período de ingresso e CRA.

-- a ) Implemente a estrutura que representa um aluno
data Aluno = Nil | Aluno String String String Int Float deriving (Show)
data ListaAlunos = ListaAlunos [Aluno] deriving (Show)

--gets
getMatricula (Aluno matricula _ _ _ _) = Just matricula
getNome (Aluno _ nome _ _ _) = Just nome
getSobrenome (Aluno _ _ sobrenome _ _) = Just sobrenome
getIngresso (Aluno _ _ _ ingresso _) = Just ingresso
getCRA (Aluno _ _ _ _ cra) = Just cra

-- b ) Implemente uma função que calcula a média dos CRAs dos alunos 
-- (dispostos em uma lista) usando o operador de foldr (você não pode usar map)
-- mediaCRAs [Aluno] = 

calculaMedia (ListaAlunos alunos) | tamanho == 0 = 0
                                  | otherwise = (foldr (+) 0 listaAlunos) / fromIntegral tamanho
    where
        listaAlunos = getCRAs (ListaAlunos alunos)
        tamanho = length listaAlunos

getCRAs (ListaAlunos alunos) = [cra | Aluno _ _ _ _ cra <- alunos]

-- c ) Implemente uma função que realiza o groupBy dos alunos por CRA. 
-- Ou seja, dada uma lista de alunos, a função retorna uma lista de pares (cra, [Aluno]), 
-- agrupando alunos com um mesmo CRA em pares cujo primeiro elemento é o CRA e o segundo é uma lista de alunos.

mySplit _ _ [] = ([], [])
mySplit igual x (y:ys)
    | igual y  = (y : mesmo, resto)
    | otherwise = ([], y:ys)
  where
    (mesmo, resto) = mySplit igual x ys

myGroupBy _ (ListaAlunos []) = ListaAlunos []
myGroupBy igual (ListaAlunos (x:xs)) =
    ListaAlunos (x : mesmo) `append` myGroupBy igual (ListaAlunos resto)
  where
    (mesmo, resto) = mySplit (igual x) x xs

append (ListaAlunos xs) (ListaAlunos ys) = ListaAlunos (xs ++ ys)