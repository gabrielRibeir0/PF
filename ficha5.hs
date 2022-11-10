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

--3
type Polinomio = [Monomio]
type Monomio = (Float,Int)

selgrau :: Int -> Polinomio -> Polinomio
selgrau n p = filter (\ (_,g) -> g==n) p

conta :: Int -> Polinomio -> Int
conta n p = length (selgrau n p)

calcula :: Float -> Polinomio -> Float
calcula x p = foldr f 0 p
   where f (c,e) v = c*x^e + v