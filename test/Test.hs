import Test.Hspec
import Control.Exception (evaluate)
import Questao1 (escolheTupla)
import Questao2 (myFloor, myCeil)
import Questao3 (MyStack(..), myStackPush, myStackPop, myStackPeek, myStackEmpty, myStackSearch)
import Questao4 (MyQueue(..), myQueueAdd, myQueueRemove, myQueuePoll, myQueueElement, myQueuePeek)
import Questao5 (Aluno(..), ListaAlunos(..), calculaMedia, myGroupBy, getCRA)

main :: IO ()
main = hspec $ do
  describe "escolheTupla" $ do
    it "Tupla mais próxima para o valor de x = 54" $ do
      escolheTupla 54 [10, 22, 28, 29, 30, 40] `shouldBe` (22, 30)

    it "Tupla mais próxima para o valor de x = 60" $ do
      escolheTupla 60 [10, 20, 30, 40, 50] `shouldBe` (10, 50)

    it "Tupla mais próxima para o valor de x = 100 para um array que possui elementos repetidos" $ do
      escolheTupla 100 [50, 50, 50, 50] `shouldBe` (50, 50)

    it "Tupla mais próxima para o valor de x = 5 para um array com valores negtivos" $ do
      escolheTupla 5 [-3, -2, 1, 3, 6] `shouldBe` (-2, 6)

    it "Tupla mais próxima para o valor de x = 0 com array com valores negativos" $ do
      escolheTupla 0 [-4, -3, -2, -1] `shouldBe` (-2, -1)

    it "retorna (0, 0) para lista vazia" $ do
      escolheTupla 50 [] `shouldBe` (0, 0)

    it "Tupla mais próxima para o valor de x = 10" $ do
      escolheTupla 10 [1, 2, 3, 4, 5, 6, 7, 8] `shouldBe` (2, 8)

    it "Tupla mais próxima para o valor de x = 15" $ do
      escolheTupla 15 [1, 4, 5, 6, 10] `shouldBe` (5, 10)
    
  describe "myFloor" $ do
    it "Floor de 3 em uma lista com um único elemento de valor 3" $ do
      myFloor 3 [3] `shouldBe` 3

    it "Teste de floor em uma lista que só possui elementos maiores que o valor fornecido de x" $ do
      evaluate (myFloor 2 [3, 4, 5, 3]) `shouldThrow` anyErrorCall

    it "Floor de 4 com um array que possui o elemento 4, então deve retornar 4" $ do
      myFloor 4 [1, 2, 3, 4, 5] `shouldBe` 4
    
    it "Floor de 4 com um array que não possui o elemento 4" $ do
      myFloor 4 [1, 2, 3, 5] `shouldBe` 3
    
  describe "myCeil" $ do
    it "Ceil de 5 em um array que só tem o elemento 5" $ do
      myCeil 5 [5] `shouldBe` 5

    it "Ceil de 6 em um array que não possui numeros maiores que 6, então deve retornar erro" $ do
      evaluate (myCeil 6 [5]) `shouldThrow` anyErrorCall

    it "Ceil de 4 em um array que possui o 4" $ do
      myCeil 4 [1, 2, 3, 4, 5] `shouldBe` 4
    
    it "Ceil de 10 em um array que não possui 10" $ do
      myCeil 10 [11, 12, 13, 14, 15] `shouldBe` 11
  
  describe "MyStack" $ do
    it "Testando adicionar um elemento a pilja" $ do
        let stack = MyStack [1 :: Integer, 2, 3]
        myStackPush 4 stack `shouldBe` MyStack [4, 1, 2, 3]

    it "Verificando se um pilha esta vazia" $ do
        myStackEmpty (MyStack []) `shouldBe` True
    
    it "Verificando se um pilha esta vazia, mas não está" $ do
        myStackEmpty (MyStack [1]) `shouldBe` False

    it "Espero um erro quando eu tentar tirar um elemento de uma pilha vazia" $ do
        evaluate (myStackPop (MyStack [])) `shouldThrow` anyErrorCall

    it "Espero um erro ao tentar olhar a cabeça de uma pilha vazia" $ do
        evaluate (myStackPeek (MyStack [])) `shouldThrow` anyErrorCall

    it "testando achar um elemento numa pilha que não possui o elemento, deve retornar -1" $ do
        myStackSearch 5 (MyStack []) `shouldBe` -1
  
  describe "MyQueue" $ do
    let queue = MyQueue [2 :: Integer, 3, 4] 

    it "Adicioando elemento da fila" $ do
      let newQueue = myQueueAdd (1 :: Integer) queue 
      newQueue `shouldBe` MyQueue [1, 2, 3, 4]
    
    it "Removendo elemento da fila" $ do
      let (removedElement, queueAfterRemove) = myQueueRemove queue
      removedElement `shouldBe` (2 :: Integer)
      queueAfterRemove `shouldBe` MyQueue [3, 4]
    
    it "Verificando a cabeça da fila" $ do
      let frontElement = myQueueElement queue
      frontElement `shouldBe` (2 :: Integer) 
    
    it "Erro ao tentar remover de uma fila vazia" $ do
      evaluate (myQueueRemove (MyQueue [])) `shouldThrow` anyErrorCall

  describe "Questao5" $ do
        let aluno1 = Aluno "123" "João" "Silva" 2020 8.5
        let aluno2 = Aluno "124" "Maria" "Santos" 2021 9.0
        let aluno3 = Aluno "125" "Pedro" "Almeida" 2020 8.0
        let aluno4 = Aluno "126" "Ana" "Souza" 2021 9.0
        let aluno5 = Aluno "127" "Lucas" "Pereira" 2021 9.0
        let listaAlunos = ListaAlunos [aluno1, aluno2, aluno3, aluno4]

        describe "calculaMedia" $ do
            it "deve calcular a média dos CRAs corretamente" $ do
                calculaMedia listaAlunos `shouldBe` 8.625

            it "deve retornar 0 para lista vazia" $ do
                calculaMedia (ListaAlunos []) `shouldBe` 0

        describe "myGroupBy" $ do
            it "deve agrupar alunos pelo CRA corretamente" $ do
                let listaAlunos2 = ListaAlunos [aluno1, aluno2, aluno3, aluno4, aluno5]
                myGroupBy (\(Aluno _ _ _ _ cra1) (Aluno _ _ _ _ cra2) -> cra1 == cra2) listaAlunos2 `shouldBe`
                    ListaAlunos [
                        Aluno "123" "João" "Silva" 2020 8.5, 
                        Aluno "124" "Maria" "Santos" 2021 9.0, 
                        Aluno "125" "Pedro" "Almeida" 2020 8.0, 
                        Aluno "126" "Ana" "Souza" 2021 9.0, 
                        Aluno "127" "Lucas" "Pereira" 2021 9.0
                    ]

            it "Retornar lista vazia para lista vazia" $ do
                myGroupBy (\(Aluno _ _ _ _ _) (Aluno _ _ _ _ _) -> False) (ListaAlunos []) `shouldBe` ListaAlunos []

            it "Agrupar alunos com o mesmo CRA" $ do
                let listaAlunos3 = ListaAlunos [aluno2, aluno4, aluno5]
                myGroupBy (\(Aluno _ _ _ _ cra1) (Aluno _ _ _ _ cra2) -> cra1 == cra2) listaAlunos3 `shouldBe`
                    ListaAlunos [
                        Aluno "124" "Maria" "Santos" 2021 9.0,
                        Aluno "126" "Ana" "Souza" 2021 9.0,
                        Aluno "127" "Lucas" "Pereira" 2021 9.0
                    ]

  
    

