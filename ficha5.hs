module Ficha5 where

--definição do all -> qualquer elemento na lista obedece ao predicado dado, se houver pelo menos um que não respeite, dá false
all' :: (a -> Bool) -> [a] -> Bool
all' _ [] = True
all' p (h:t) | p h = all' p t
             | otherwise = False

--span com tupling, fazendo apenas uma travessia da lista
span' :: (a -> Bool) -> [a] -> ([a],[a])
span' _ [] = ([],[])
span' p (h:t) | p h = (h:lt,ld)
              | otherwise = ([], h:t)
    where (lt,ld) = span p t

deleteBy' :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy' _ _ [] = []
deleteBy' p n (h:t) | p n h = t
                    | otherwise = h : deleteBy' p n t

maioresQueDois :: [Int] -> [Int] --podemos usar filter
maioresQueDois [] = []
maioresQueDois (x:xs) | x > 2 = x : maioresQueDois xs
                      | otherwise = maioresQueDois xs

maioresQueDois' :: [Float] -> [Float]
maioresQueDois' l = filter (>2) l

--2) tipos para representar monómios e polinómios
type Polinomio = [Monomio]
type Monomio = (Float,Int)

--a)função que seleciona os monómios de um dado grau de um polinómio
selgrau :: Int -> Polinomio -> Polinomio
selgrau n p = filter (\ (_,g) -> g==n) p

--b)função que indica quantos monómios de grau no polinómio
conta :: Int -> Polinomio -> Int
conta n p = length (selgrau n p)

--c)função que indica o grau de um polinómio
grau :: Polinomio -> Int
grau p = maximum (map snd p)

--e)função que calcula o valor de um polinómio para um dado valor de x
calcula :: Float -> Polinomio -> Float
calcula x p = foldr f 0 p
   where f (c,e) v = c*x^e + v

--3)tipo para representar matrizes
type Mat a = [[a]]

--a)função que testa se uma matriz está bem construída (todas as linhas têm a mesma dimensão)
dimOK :: Mat a -> Bool
dimOK m = let cl = length (head m)
          in and (map(\l -> length l == cl) m)
          --com all -> all (\l -> lenght l == cl) m
          --fazer com fold

--b)função que calcula a dimensão de uma matriz
dimMat :: Mat a -> (Int,Int)
dimMat m = (length m,length (head m))

--c)função que adiciona 2 matrizes
addMat :: Num a => Mat a -> Mat a -> Mat a
addMat [] [] = []
addMat (h:t) (x:xs) = zipWith (+) h x : addMat t xs

--d)função que calcula a transposta de uma matriz
transpose :: Mat a -> Mat a
transpose [] = []
transpose m | null (head m) = []
            | otherwise = firstCol m : transpose (otherCols m)

firstCol :: Mat a -> [a]
firstCol m = map head m

otherCols :: Mat a -> Mat a
otherCols m = map tail m

--e)função que calcula o produto de 2 matrizes
multMat :: Num a => Mat a -> Mat a -> Mat a
multMat m1 m2 = multMat' m1 (transpose m2)

multMat' :: Num a => Mat a -> Mat a -> Mat a 
multMat' [] _ = []
multMat' _ [] = []
multMat' (a:b) m = multLinha a m : multMat' b m

multLinha :: Num a => [a] -> Mat a -> [a]
multLinha _ [] = []
multLinha l (a:as) = sum(zipWith (*) l a) : multLinha l as