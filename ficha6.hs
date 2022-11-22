module Ficha6 where

--1) tipo para representar árvores binárias
data BTree a = Empty
             | Node a (BTree a) (BTree a)
             deriving Show
--ex
a ::BTree Int
a = Node 5 (Node 3 (Empty) (Node 2 (Empty) (Empty))) (Node 4 (Empty) (Empty))

--ex
ap ::BTree Int
ap = Node 5 (Node 3 (Node 2 (Node 1 (Empty) (Empty)) (Empty)) (Node 4 (Empty) (Empty))) (Node 10 (Node 7 (Empty) (Node 8 (Empty) (Empty))) (Node 12 (Empty) (Empty))) 
--a)função para calcular a altura da árvore
altura ::BTree a -> Int
altura Empty = 0
altura (Node x e d) = 1 + max (altura e) (altura d)

--b)função para calcular o número de nodos da árvore
contaNodos ::BTree a -> Int
contaNodos Empty = 0
contaNodos (Node x e d) = 1 + contaNodos e + contaNodos d

--c)função para calcular o número de folhas da árvore (nodos sem descendentes)
folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node _ Empty Empty) = 1
folhas (Node _ e d) = folhas e + folhas d

--d)função que remove de uma árvore todos os elementos a partir de uma dada profundidade
prune :: Int -> BTree a -> BTree a
prune 0 _ = Empty
prune p Empty = Empty
prune p (Node x e d) = Node x (prune (p-1) e) (prune (p-1) d)

--e)função, que dado um caminho (False - esquerda, True - direita) e uma árvore, dá a lista com a informação dos nodos por onde o caminho passa
path :: [Bool] -> BTree a -> [a]
path _ Empty = []
path [] (Node x e d) = [x]
path (h:t) (Node x e d) | h == True = x : path t d
                        | otherwise = x : path t e

--f)função que dá a árvore simétrica

--g)função que realiza a função zipWith para árvores binárias

--h)função que generaliza a função unzip (no caso de triplos) para árvores binárias

--2)árvores binárias de procura
--a)função que determina o menor elemento de uma árvore binária de procura não vazia
minimo :: Ord a => BTree a -> a
minimo (Node x Empty d) = x
minimo (Node x e d) = minimo e

--b) função que remove o menor elemento de uma árvore binária de procura não vazia
semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node x (Node y Empty r) d) = Node x Empty d
semMinimo (Node x e d ) = Node x (semMinimo e) d

--c)função que calcula com uma única travessia da árvore o resultado das duas funções anteriores

--d)função que remove um elemento de uma árvore binária de procura, usando a função anterior

--3)estrutura de dados para guardar informação sobre uma turma de alunos
type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String

data Regime = ORD | TE | MEL 
    deriving Show

data Classificacao = Aprov Int
                   | Rep
                   | Faltou
    deriving Show
type Turma = BTree Aluno --  ́arvore binária de procura (ordenada por número)

--a)função para verificar se um aluno com um dado número está inscrito
inscNum :: Numero -> Turma -> Bool
inscNum _ Empty = False
inscNum n (Node (num,_,_,_) e d) | num == n = True 
                                 | num > n = inscNum n e
                                 | otherwise = inscNum n d

--b)função para verificar se um aluno com um dado nome está inscrito
inscNome :: Nome -> Turma -> Bool
inscNome _ Empty = False
inscNome n (Node (_,nome,_,_) e d) | n == nome = True
                                   | otherwise = inscNome n e || inscNome n d
            
--c)função que lista o número e nome dos alunos trabalhadores-estudantes (ordenados por número)
trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (n,nome,TE,_) e d) = trabEst e ++ trabEst d ++ [(n,nome)] 
trabEst (Node (_,_,_,_) e d) = trabEst e ++ trabEst d

--d)função que calcula a classificação de um aluno (se não estiver inscrito retorna Nothing)
nota :: Numero -> Turma -> Maybe Classificacao
