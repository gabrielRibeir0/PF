module Ficha7 where

--1)tipo de dados para representar esxpressões inteiras
data ExpInt = Const Int
            | Simetrico ExpInt
            | Mais ExpInt ExpInt
            | Menos ExpInt ExpInt
            | Mult ExpInt ExpInt

--a) função para calcular o valor de uma expressão
calcula :: ExpInt -> Int
calcula (Const i) = i
calcula (Simetrico e) = -(calcula e)
calcula (Mais e1 e2) = calcula e1 + calcula e2
calcula (Menos e1 e2) = calcula e1 - calcula e2
calcula (Mult e1 e2) = calcula e1 * calcula e2

--b) função infixa que infixa (Mais (Const 3) (Menos (Const 2) (Const 5))) dê "(3 + (2-5))"
infixa :: ExpInt -> String
infixa (Const i) = show i
infixa (Simetrico e) = "(" ++ infixa e ++ ")"
infixa (Mais e1 e2) = '(' : infixa e1 ++ " + " ++ infixa e2 ++ ")"
infixa (Menos e1 e2) = '(' : infixa e1 ++ " - " ++ infixa e2 ++ ")"
infixa (Mult e1 e2) = '(' : infixa e1 ++ " * " ++ infixa e2 ++ ")"

--c)função de conversão para strings, mas que dê "3 2 5 - +"
posfixa :: ExpInt -> String
posfixa (Const i) = show i
posfixa (Simetrico e) = "(" ++ posfixa e ++ ")" 
posfixa (Mais e1 e2) = posfixa e1 ++ " " ++ posfixa e2 ++ " + "
posfixa (Menos e1 e2) = posfixa e1 ++ " " ++ posfixa e2 ++ " - "
posfixa (Mult e1 e2) = posfixa e1 ++ " " ++ posfixa e2 ++ " * "

--2)tipo para representar árvores irregulares (rose trees)
data RTree a = R a [RTree a]
--ex
rt = R 2 [R 3 [R 2 []
             , R 4 []]
        , R 7 []
        , R 8 [R 4 []]
        ]

--a)função que soma os elementos da árvore
soma :: Num a => RTree a -> a
soma (R x l) = x  + sum(map soma l) 

--b)função que calcula a altura da árvore
altura :: RTree a -> Int
altura (R x []) = 1
altura (R x l) = maximum(map altura l)

--c)função que remove de uma árvore todos os elementos a partir de uma dada profundidade
prune :: Int -> RTree a -> RTree a
prune 