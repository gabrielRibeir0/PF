module Ficha6 where

--1) tipo para representar árvores binárias
data BTree a = Empty
             | Node a (BTree a) (BTree a)

instance Show a => Show (BTree a) where
    show :: Show a =>BTree a -> String
    show Empty = "*"
    show (Node e l r) = "(" ++ show l ++ " <-" ++ show e ++ "-> " ++ show r ++ ")"

a1 = Node 5 (Node 3 Empty Empty) (Node 7 Empty (Node 9 Empty Empty))
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
path (h:t) (Node x e d) | h = x : path t d
                        | otherwise = x : path t e

--f)função que dá a árvore simétrica
mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node x e d) = Node x (mirror e) (mirror d)

--g)função que realiza a função zipWith para árvores binárias
zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f (Node x e d) (Node y l r) = Node (f x y) (zipWithBT f e l) (zipWithBT f d r)
zipWithBT _ _ _ = Empty

--h)função que generaliza a função unzip (no caso de triplos) para árvores binárias

--2)árvores binárias de procura
--a)função que determina o menor elemento de uma árvore binária de procura não vazia
minimo :: Ord a => BTree a -> a
minimo (Node x Empty d) = x
minimo (Node x e d) = minimo e

--b) função que remove o menor elemento de uma árvore binária de procura não vazia
semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node x Empty d) = d
semMinimo (Node x e d ) = Node x (semMinimo e) d

--c)função que calcula com uma única travessia da árvore o resultado das duas funções anteriores
minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin (Node x Empty d) = (x,d)
minSmin (Node x e d) = (a,Node x b d)
    where (a,b) = minSmin e

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
inscNum n (Node (num,_,_,_) e d) | n == num = True 
                                 | n < num = inscNum n e
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
nota _ Empty = Nothing
nota n (Node (num,_,_,c) e d) | n == num = Just c
                              | n < num = nota n e
                              | otherwise = nota n d

--e)função que calcula a percentagem de alunos que faltaram à avaliação
percFaltas :: Turma -> Float
percFaltas t = (numFaltas t / numAlunos t) * 100

numAlunos :: Turma -> Float
numAlunos Empty = 0
numAlunos (Node x e d) = 1 + numAlunos e + numAlunos d

numFaltas :: Turma -> Float         
numFaltas Empty = 0
numFaltas (Node (_,_,_,Faltou) e d) = 1 + numFaltas e + numFaltas d
numFaltas (Node x e d) = numFaltas e + numFaltas d

--f)função que calcula a média das notas dos alunos que passaram
mediaAprov :: Turma -> Float
mediaAprov t = sumNotas t /numAprov t

numAprov :: Turma -> Float
numAprov Empty = 0
numAprov (Node (_,_,_,Aprov _) e d) = 1 + numAprov e + numAprov d
numAprov (Node x e d) = numAprov e + numAprov d

sumNotas :: Turma -> Float
sumNotas Empty = 0
sumNotas (Node (_,_,_,Aprov n) e d) = fromIntegral n + sumNotas e + sumNotas d
sumNotas (Node x e d) = sumNotas e + sumNotas d

--g)função que calcula o rácio de alunos aprovados por avaliados. Apenas uma travessia da árvore

remove :: Ord a => a -> BTree a -> BTree a
remove _ Empty = Empty
remove x (Node a l r)
    | x > a = Node a l (remove x r)
    | x < a = Node a (remove x l) r
    | otherwise = 
        case (l,r) of 
            (Empty, r) -> r
            (l, Empty) -> l
            (l, r) -> let (min, sMin) = minSMin r in Node min l sMin

minSMin :: BTree a -> (a, BTree a)
minSMin (Node e Empty r) = (e, r)
minSMin (Node e l r) = let (min, sMin) = minSMin l in (min, Node e sMin r)