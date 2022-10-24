--1) definição recursiva da função enumFromTo :: Int -> Int -> [Int] que constroi a lista dos números inteiros compreendidos entre dois limites
enumFromTo' :: Int -> Int -> [Int]
enumFromTo' ini fim | ini > fim = []
                    | otherwise = ini : enumFromTo' (ini+1) fim 

--2) definição recursiva da função enumFromThenTo :: Int -> Int-> Int -> [Int] que constrói a lista dos números inteiros compreendidos entre dois limites espaçados de um valor constante
enumFromThenTo' :: Int -> Int -> Int -> [Int]
enumFromThenTo' ini esp fim | ini > fim && esp >= ini || ini < fim && esp < ini = []
                            | otherwise = ini : enumFromThenTo' esp (2*esp - ini) fim

--3) definição recursiva da função (++) :: [a] -> [a] -> [a] que concatena duas listas
m :: [a] -> [a] -> [a]
m [] l = l
m (x:xs) l = x : m xs l

--4) definição recursiva da função (!!) :: [a] -> Int -> a que dada uma lista e um inteiro, calcula o elemento da lista que se encontra nessa posição (1ª pos = 0)
pos :: [a] -> Int -> a
pos (x:_) 0 = x
pos (_:xs) n | n > 0 = pos xs (n - 1)

--5) definição recursiva da função reverse :: [a] -> [a] que dada uma lista, calcula uma lista com os elementos dessa lista pela ordem inversa
reverse'  :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

--6) definição recursiva da função take :: Int -> [a] -> [a] que dado um inteiro n e uma lista l calcula a lista com os (no máximo) n primeiros elementos de l
take' :: Int -> [a] -> [a]
take' _ [] = []
take' n (h:l) | n <= 0 = []
              | otherwise = h : take' (n-1) l

--7) definição recursiva da função drop :: Int -> [a] -> [a] que dado um inteiro n e uma lista l calcula a lista sem os (no máximo) n primeiros elementos de l
drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' n (h:l) | n <= 0 = h:l
              | otherwise = drop' (n-1) l

--8) definição recursiva da função zip :: [a] -> [b] -> [(a,b)] que constrói uma lista de pares a partir de duas listas
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (a:as) (b:bs) = (a, b) : zip' as bs 

--9) definição recursiva da função replicate :: Int -> a -> [a] que dado um inteiro n e um elemento x constrói uma lista com n elementos, todos iguais a x
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate (n-1) x

--10) definição recursiva da função intersperse :: a -> [a] -> [a] que dado um elemento e uma lista, constrói uma lista em que o elemento fornecido  ́e intercalado entre os elementos da lista fornecida
intersperse' :: a -> [a] -> [a]
intersperse' _ [] = []
intersperse' _ [x] = [x]
intersperse' a (h:t) = h : a : intersperse' a t

--11) definição recursiva da função group :: Eq a => [a] -> [[a]] que agrupa elementos iguais e consecutivos de uma lista
group' :: Eq a => [a] -> [[a]]
group' [] = []
group' [x] = [[x]]
group' (h:t) | elem h (head s) = (h : (head s)) : tail s 
            | otherwise = [h] : s
    where s = group' t

--12)definição recursiva da função concat :: [[a]] -> [a] que concatena as listas de uma lista
concat' :: [[a]] -> [a]
concat' [] = []
concat' (h:t) = h ++ concat' t

--13)definição recursiva da função que calcula a lista dos prefixos de uma lista
inits :: [a] -> [[a]]
inits [] = [[]]
inits l = inits (init l) ++ [l]

--14)definição recursiva da função que calcula a lista dos sufixos de uma lista
tails :: [a] -> [[a]]
tails [] = [[]]
tails l = l : tails (tail l)

--15)função que recebe uma lista de listas e produz a lista com o primeiro elemento de cada lista
heads :: [[a]] -> [a]
heads [] = []
heads (h:t) | null h = heads t
            | otherwise = head h : heads t

--16)função que recebe uma lista e conta o total de elementos (todas as listas)
total :: [[a]] -> Int
total [] = 0
total (h:t) = len h + total t
    where len [] = 0
          len (x:xs) = 1 + len xs

--17)função que recebe uma lista de triplos e produz a lista de pares com o primeiro e o terceiro elemento de cada triplo
fun :: [(a,b,c)] -> [(a,c)]
fun [] = []
fun ((a,b,c):t) = (a,c) : fun t

--18)função que receve uma lista de triplos e concatena as strings que estão na primeira componente dos triplos
cola :: [(String,b,c)] -> String
cola [] = ""
cola ((s,b,c):t) = s ++ cola t

--19)função que recebe o ano, a idade e um alista de pares com o nome e ao ano de nascimento de cada pessoa, e 
--   devolve a lista de nomes das pessoas que nesse ano atingirão ou já ultrapassaram a idade dada
idade :: Int -> Int -> [(String,Int)] -> [String]
idade _ _ [] = []
idade a i ((n,an):t) | a - an >= i = n : idade a i t
                     | otherwise = idade a i t
--28)
{-rem :: Eq a => [a] -> [a] ->
rem 
remAux (x:xs) (y:ys) ac | x == y = remAux
                        | otherwise = remAux xs (y:ys) x : ac -}
