module Ficha6 where

--1) tipo para representar árvores binárias
data BTree a = Empty
             | Node a (BTree a) (BTree a)
             deriving Show
--ex
a ::BTree Int
a = Node 5 (Node 3 (Empty) (Node 2 (Empty) (Empty))) (Node 4 (Empty) (Empty))

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