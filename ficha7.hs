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
prune 0 (R e es) = R e []
prune n (R e es) = R e (map (prune (n - 1)) es)

--d)função que gera a árvore simátrica
mirror :: RTree a -> RTree a
mirror (R e es) = R e (map mirror (reverse es))

--e)função que faz a travessia postorder da árvore (esquerda, direita e depois raiz)

--3)tipo de dados para Leaf Trees (a informação está apenas nas extremidades)
data LTree a = Tip a | Fork (LTree a) (LTree a)

--a)função que soma as folhas de uma árvore
ltSum :: Num a => LTree a -> a
ltSum (Tip f) = f
ltSum (Fork e d) = ltSum e + ltSum d

--b)função que lista as folhas de uma árvore (da esquerda para a direita)
listaLT :: LTree a -> [a]
listaLT (Tip f) = [f]
listaLT (Fork e d) = listaLT e ++ listaLT d

--c)função que calcula a altura de uma árvore
ltHeight ::LTree a -> Int
ltHeight (Tip f) = 0
ltHeight (Fork e d) = 1 + max (ltHeight e) (ltHeight d)

--4)tipo de dados para representar Full Trees (a informação não está só nos nodos mas também nas folhas, não tendo de ser do mesmo tipo)
data FTree a b = Leaf b | No a (FTree a b) (FTree a b)

--árvores binárias
data BTree a = Empty
             | Node a (BTree a) (BTree a)
             deriving Show

--a)função que separa uma full tree em duas árvores de tipos diferentes
splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree (Leaf f) = (Empty, Tip f)
splitFTree (No x e d) = (Node x eb db, Fork el dl)
    where (eb, el) = splitFTree e
          (db, dl) = splitFTree d 

--b)função que junta árvores numa só sempre que sejam compatíveis
joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
joinTrees (Empty) (Tip f) = Just (Leaf f)
joinTrees (Node i e d) (Fork a b) =
    case (joinTrees e a, joinTrees d b) of (Just x, Just y) -> Just (No i x y)
                                           _ -> Nothing
joinTrees _ _ = Nothing