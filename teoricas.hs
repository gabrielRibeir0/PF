module Teorica where
import System.Random
import System.IO.Error

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

{-Tipos Algébricos
Ex
data List a = Nil | Cons a (List a)
a lista [1,2,3] pode ser 1 : 2 : 3 : []
com o contrutor Cons 1(Cons 2 (Cons 3 Nil))

Ex data [a] = []
            | (:) a [a]

-Árvores binárias
Uma arvore binária -> ou é vazia
                   -> ou tem um elemento e a duas sub-estruturas que também são árvores

data BTree a = Empty
             | Node a (BTree a) (BTree a)
                        |           |
                        v           v
         arvore lado esquerdo     arvore lado direito

terminologia-}
data BTree a = Empty
             | Node a (BTree a) (BTree a)
--contar o número de nodos que tem uma árvore
conta :: BTree a -> Int
conta Empty = 0
conta (Node x e d) = 1 + conta e --nodo atual + nodos da esquerda
                       + conta d -- + nodos da direita

--somar todos os nodos de um árvore de números
sumBT :: Num a => BTree a -> a
sumBT Empty = 0
sumBT (Node x e d) = x + sumBT e -- número do nodo atual + soma dos números da esquerda 
                       + sumBT d -- + soma dos números da direita

--calcular a altura de uma árvore
altura :: BTree a -> Int
altura Empty = 0
altura (Node x e d) = 1 + max (altura e) (altura d) --1 (se não é vazia tem pelo menos altura 1) + maior entre altura da árvore direita e a árvore esquerda

--funções map e zip para árvores binárias
mapBT :: (a -> b) -> BTree a -> BTree b
mapBT f Empty = Empty
mapBT f (Node x e d) = Node (f x) (mapBT f e) (mapBT f d)

zipBT :: BTree a -> BTree b -> BTree (a,b)
zipBT (Node x e d) (Node y l r) = Node (x,y) (zipBT e l) (zipBT d r)
zipBT _ _ = Empty

{-Travessia de Arvores Binárias
As principais estratégias para percorrer uma árvore são:-}

--Travessia preorder: visita a raiz, depois a árvore esqueda e a seguir a árvore direita
preorder :: BTree a -> [a]
preorder Empty = []
preorder (Node x e d) = [x] ++ preorder e ++ preorder d

--Travessia inorder: visita árvore esquerda, depois a raiz e depois árvore direita
inorder :: BTree a -> [a]
inorder Empty = []
inorder (Node x e d) = inorder e ++ [x] ++ inorder d

--Travessia postorder: visitar árvore esquerda, depois árvore direita e por fim a raiz
postorder :: BTree a -> [a]
postorder Empty = []
postorder (Node x e d) = postorder e ++ postorder d ++ [x]

{-Árovres binárias de procura (ou de pesquisa)
Uma árvore binária em que o valor de cada nodo é maior do que os nodos à sua esquerda e menor do que os nodos à sua direita 
Verifica as seguintes condições:
-a raiz da árvore é maior do que todos os elementos do ramo da esquerda
-a raiz da árvore é menor do que todos os elementos do ramo da direita
-
O formato da árvore depende da ordem em que os elementos são inseridos
Quanto menor a altura da árvore de procura, melhor-}

--Testar se um elemento pertence a uma árvore binária de procura
elemBT :: Ord a => a -> BTree a -> Bool
elemBT x Empty = False
elemBT x (Node y e d) | x == y = True           --se não fosse de procura
                      | x < y = elemBT x e      -- otherwise = elemBT x e || elemBT x d
                      | otherwise = elemBT x d

--Inserir um elemento numa árvore binária de procura
insereBT :: Ord a => a -> BTree a -> BTree a
insereBT x Empty = Node x Empty Empty
insereBT x (Node y e d) | x == y = Node y e d
                        | x < y = Node y (insereBT x e) d
                        | otherwise = Node y e (insereBT x d)

--criar uma árvore binária de procura a partir de uma lista
listToBT :: Ord a => [a] -> BTree a
listToBT [] = Empty
listToBT (x:xs) = insereBT x (listToBT xs)

-- usando foldr -> listToBT l = foldr insereBT Empty l 
{-função com acumulador
listToBT l = listBTAc l Empty

listBTAc [] ac = ac
listBTAc (x:xs) ac = listBTAc xs (insereBT x ac)

que pode ser escrita usando foldl
listToBT l = foldl (\ac x -> insereBT x ac) Empty l 
ou  listToBT l = foldl (flip insereBT) Empty l -}

{-Árvores balanceadas
Uma árvore binária é balenceada (ou equilibrada) se é vazia, ou se:
- as alturas das sub-árvores esquerda e direita diferem no máximo em uma unidade
-ambas as sub-árvores são balenceadas-}

--Testar se uma árvore é balenceada
testBal :: BTree a -> Bool
testBal Empty = True
testBal (Node x e d) = abs (altura e - altura d) <= 1 && testBal e && testBal d

--Balencear uma árvore -> gerar uma lista ordenada com os seus elementos e depois construir a árvore a partir da lista
balencear :: BTree a -> BTree a
balencear t = constroi (inorder t)

constroi :: [a] -> BTree a
constroi [] = Empty
constroi l = let n = length l
                 (l1,x:l2) = splitAt (n `div` 2) l
             in Node x (constroi l1) (constroi l2)

--ou 
balance :: BTree a -> BTree a
balance t = constr (inorder t , length (inorder t))

constr :: ([a],Int) -> BTree a
constr ([],0) = Empty
constr (l,n) = let a = n `div` 2
                   (l1,x:l2) = splitAt a l
                in Node x (constr (l1,a)) (constr (l1,n-a-1))


niveis :: BTree a -> [a]
niveis Empty = []
niveis (Node x e d) = x: aux [e,d]

aux :: [BTree a] -> [a]
aux [] = []
aux (Empty : t) = aux t
aux ((Node x e d):t) = x : aux (t++[e,d])  

{-Árvores irregulares (rose trees)
Nas árvores irregulares cada nodo pode ter um número variável de descendentes.-}
data RTree a = R a [RTree a]
    deriving Show

--Contar elementos de uma árvore irregular
contaRT :: RTree a -> Int 
contaRT (R x l) = 1 + sum(map contaRT l)

alturaRT :: RTree a -> Int
alturaRT (R x []) = 1
alturaRT (R x l) = 1 + maximum(map alturaRT l)

niveisRT :: RTree a -> [a]
niveisRT t = auxRT [t]

auxRT :: [RTree a] -> [a]
auxRT [] = []
auxRT ((R x l):t) = x : auxRT (t++l)

elemRT :: Eq a => a -> RTree a -> Bool
elemRT x (R y l) | x == y = True
                 | otherwise = any (==True) (map (elemRT x) l)

{-Leaf Trees
Árvores binárias em que a informação está apenas nas folhas da árvore. Os nós intermediários não têm informação-}
data LTree a = Tip a
             | Fork (LTree a) (LTree a)

--
folhas :: LTree a -> [a]
folhas (Tip x) = [x]
folhas (Fork e d) = folhas e ++ folhas d

folhasNivel :: LTree a -> [(a,Int)]
folhasNivel (Tip x) = [(x,1)]
folhasNivel (Fork e d) = map (\(x,n) -> (x,n+1)) (folhasNivel e ++ folhasNivel d)

--função que reconstrói uma árvore (inversa da de cima)
reconstroi :: [(a,Int)] -> LTree a
reconstroi l = fst (recaux 1 l)

recaux :: Int -> [(a,Int)] -> (LTree a, [(a,Int)])
recaux n ((a,x):t) | n == x = (Tip a, t)
                   | n < x = (Fork e d,l2)
    where (e,l1) = recaux (n+1) ((a,x):t)
          (d,l2) = recaux (n+1) l1

{-Full Trees 
Árvores binárias que têm informação nos nós intermédios e nas folhas. A informação dos nós e das folhas pode ser de tipo diferente
-}
data FTree a b = Leaf b
                | No a (FTree a b) (FTree a b)

--Classes
{-
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x == y = not (x /= y)
    x /= y = not (x == y)
-}
--tipo de dados para representar números naturais
data Nat = Zero
         | Suc Nat --não podemos usar == para comparar assim
  --  deriving Eq    assim já

--ou definir a instância
instance Eq Nat where
    (Suc x) == (Suc y) = x==y
    Zero == Zero = True
    _ == _ = False

--tipo de dados para representar horas
data Time = AM Int Int | PM Int Int | Total Int Int
  --  deriving Eq
--assim, PM 1 30 e Total 13 30 daria False, mas queremos True
--então teremos de definir a instância

--função para converter as horas em minutos
minutos :: Time -> Int 
minutos (AM h m) = 60*h + m
minutos (PM h m) = 60*(h+12) + m
minutos (Total h m) = 60*h + m

instance Eq Time where
 t1 == t2 = minutos t1 == minutos t2

--Monads e monad IO
--funções e tipos não puros que envolvem interações
--função putStr que escreve uma string
puStr :: String -> IO ()
puStr (x:xs) = (putChar x) >> puStr xs
puStr [] = return ()

--função getLine que lê uma string
geLine :: IO String
geLine = getChar >>= (\x -> if x == '\n' then return [] else getLine >>= (\xs -> return (x:xs)))

--Jogo de adivinha 
--é gerado um número aleatório entre 1 e n
--o jogador tenta adivinhar o número e o computador responde se o número é baixo, se é alto ou se acertou, contanto o número de tentativas

adivinha :: IO ()
adivinha = do putStr "Qual o número máximo com que quer jogar?"
              n <- getLine
              n1 <- tryIOError (readIO n)
              case n1 of
                Left _ -> do putStr "Erro. Valor assumido -> 10"
                             x <- randomRIO (1,10)
                             y <- joga x 0
                             putStrLn ("Acertou. Usou " ++ show y ++ " tentativas.")
                Right r -> do x <- randomRIO (1,r)
                              y <- joga x 0
                              putStrLn ("Acertou. Usou " ++ show y ++ " tentativas.")

joga :: Int -> Int -> IO Int
joga x n = do putStr "Palpite: "
              s <- getLine
              r <- readIO s
              if r == x
              then return (n+1)
              else if r > x
                   then putStr "É alto ..." >> joga x (n+1)
                   else putStr "É baixo ..." >> joga x (n+1)