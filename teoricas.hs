sumTPar :: [(Int,Int)] -> Int
sumTPar (_:_:(x,y):_) = x+y

elem' :: Eq a => a -> [a] -> Bool
elem' x [] = False
elem' x (h:t) | x == h = True
             | otherwise = elem' x t

cola :: [a] -> [a] -> [a]
cola [] l = l
cola (x:xs) l = x : cola xs l

-- m = (++)
m :: [a] -> [a] -> [a]
m [] l = l
m (x:xs) l = x : m xs l

reverse'  :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- p = (!!)
p :: [a] -> Int -> a
p (x:xs) 0 = x
p (x:xs) n | n > 0 = p xs (n - 1)

take' :: Int -> [a] -> [a]
take' 0 l = []
take' n (h:t) = h : take' (n -1) t

somas :: [(Int,Int)] -> (Int,Int)
somas [] = (0,0)
somas ((x,y):t) = (x + a, y + b)
    where (a,b) = somas t
-- ou somas((x,y):t) = (x + fst(somas t) , y + snd(somas t))

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

unzip' :: [(a,b)] -> ([a],[b])
unzip' [] = ([],[])
unzip' ((x,y):t) = (x : fst(unzip' t), y : snd(unzip' t))
-- ou unzip' = (x : a, y : b)
      -- where (a,b) = unzip t 

splitAt' :: Int -> [a] -> ([a],[a])
splitAt' n l | n <= 0 = ([] , l)
splitAt' n [] = ([],[])
splitAt' n (h:t) = (h : a,b)   
    where (a,b) = splitAt' (n-1) t


insert :: Ord a => a -> [a] -> [a] -- recebe uma lista já ordenada
insert x [] = [x]
insert x (h:t) | x <= h = x:h:t 
               | otherwise = h : insert x t

isort :: Ord a => [a] -> [a]
isort [] = []
isort (h:t) = insert h (isort t)

qSort :: (Ord a) => [a] -> [a]
qSort [] = []
qSort (x:xs) = (qSort l1) ++ [x] ++ qSort l2
    where (l1,l2) = parte x xs

parte :: (Ord a) => a -> [a] -> ([a],[a])
parte x [] = ([],[])
parte x (y:ys) | y < x = (y : a , b)
               | otherwise = (a,y:b)
    where (a,b) = parte  x ys

mSort :: [Int] -> [Int]
mSort [] = []
mSort [x] = [x]
mSort l = merge (mSort l1) (mSort l2)
    where (l1,l2) = splitAt (div (length l) 2) l

merge :: [Int] -> [Int] -> [Int]
merge [] l = []
merge l [] = []
merge (x:xs) (y:ys) | x < y = x : merge xs (y:ys)
                    | otherwise = merge (x:xs) ys

inverte :: [a] -> [a]          -- reverseV2
inverte l = inverteAc l []

inverteAc :: [a] -> [a] -> [a]
inverteAc (x:xs) ac = inverteAc xs (x:ac)
inverteAc [] ac = ac

maximo :: (Ord a) => [a] -> a
maximo (x:xs) = maximoAc xs x

maximoAc :: Ord a => [a] -> a -> a
maximoAc (x:xs) ac | x > ac = maximoAc xs x
                   | otherwise = maximoAc xs ac
maximoAc [] ac = ac
--maximoAc (x:xs) ac = maximoAc xs (max x ac)

fact :: Integer -> Integer
fact n = factAc n 1
    where factAc n ac | n> 0 = factAc (n-1) (n * ac)
          factAc 0 ac = ac

