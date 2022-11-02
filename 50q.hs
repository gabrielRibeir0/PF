--1) definição recursiva da função enumFromTo :: Int -> Int -> [Int] que constroi a lista dos números inteiros compreendidos entre dois limites
enumFromTo' :: Int -> Int -> [Int]
enumFromTo' ini fim | ini > fim = []
                    | otherwise = ini : enumFromTo' (ini+1) fim 

--2) definição recursiva da função enumFromThenTo :: Int -> Int-> Int -> [Int] que constrói a lista dos números inteiros compreendidos entre dois limites espaçados de um valor constante
enumFromThenTo' :: Int -> Int -> Int -> [Int]
enumFromThenTo' ini esp fim | ini > fim && esp >= ini || ini < fim && esp <= ini = []
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

--sem recursividade (não dá para fazer no teste)
inits' :: [a] -> [[a]]
inits' l = [take x l | x <- [0..length l]]

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

--20)definição recursiva da função que dado um valor n e outro m constrói a lista [n^0,...,n^m-1]
powerEnumFrom :: Int -> Int -> [Int]
powerEnumFrom n 0 = []
powerEnumFrom n m = powerEnumFrom n (m-1) ++ [n^(m-1)]

--21)definição recursiva da função que dado um inteiro n >= 2 diz se n é primo.
isPrime :: Int -> Bool
isPrime n | n >= 2 = primo n 2
          | otherwise = False

primo :: Int -> Int -> Bool
primo n m | m^2 > n = True
          | mod m n == 0 = False
          | otherwise = primo n (m+1)

--22)definição recursiva da função que testa se uma lista é prefixo de outra
isPrefixOf' :: Eq a => [a] -> [a] -> Bool
isPrefixOf' [] _ = True
isPrefixOf' _ [] = False
isPrefixOf' (h:t) (x:xs) = h == x && isPrefixOf' t xs

--23)definição recursiva da função que testa se uma lista é sufixo de outra
isSuffixOf' :: Eq a => [a] -> [a] -> Bool
isSuffixOf' [] _ = True
isSuffixOf' _ [] = False
isSuffixOf' s l = last s == last l && isSuffixOf' (init s) (init l) 
-- ou isSuffixOf' l (h:t) = l == (h:t) || isSuffixOf' l t

--24)definição recursiva da função que testa se os elementos de uma lista ocorrem noutra pela mesma ordem relativa
isSubsequenceOf' :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf' [] _ = True
isSubsequenceOf' _ [] = False
isSubsequenceOf' (h:t) (x:xs) = h == x && isSubsequenceOf' t xs || isSubsequenceOf' (h:t) xs

--25)definição recursiva da função que calcula a lista de posições em que um dado elemento ocorre numa lista
elemIndices' :: Eq a => a -> [a] -> [Int]
elemIndices' _ [] = []
elemIndices' e l = contador e l 0

contador :: Eq a => a -> [a] -> Int -> [Int]
contador _ [] _ = []
contador e (h:t) i | e == h = i : contador e t (i+1)
                   | otherwise = contador e t (i+1)

--26)definição recursiva da função que calcula uma lista com os mesmos elementos da recebida, sem repetições
nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (h:t) | elem h t = nub' t
           | otherwise = h: nub' t

--27)definição recursiva da função que retorna a lista resultante de remover (a primeira ocorrência de) um dado elemento de uma lista
delete' :: Eq a => a -> [a] -> [a]
delete' _ [] = []
delete' x (h:t) | x == h = t
                | otherwise = h : delete' x t

--28)definição recursiva da função que retorna a lista resultante de remover (as primeiras ocorrências) dos elementos da segunda lista da primeira
del :: Eq a => [a] -> [a] -> [a]
del l [] = l
del [] l = []
del l (h:t) = delete' h l ++ del l t
-- ou del l (h:t) = del (delete' h l) t

--29)definição recursiva da função que retorna a lista resultante de acrescentar à primeira lista os elementos da segunda que não ocorrem na primeira
union' :: Eq a => [a] -> [a] -> [a]
union' l [] = l
union' l (h:t) | h `elem` l = union' l t
               | otherwise = union' (l ++ [h]) t

--30)definição recursiva da função que retorna a lista resultante de remover da primeira lista os elementos que não pertencem à segunda
intersect' :: Eq a => [a] -> [a] -> [a]
intersect' [] _ = []
intersect' (h:t) l | h `elem` l = h : intersect' t l 
                   | otherwise = intersect' t l 

--31)definição recursiva da função que dado um elemento e uma lista ordenada retorna a lista resultante de inserir ordenadamente esse elemento na lista
insert' :: Ord a => a -> [a] -> [a]
insert' x [] = [x]
insert' x (h:t) | x <= h = x:h:t 
                | otherwise = h : insert' x t 

--32)definição recursiva da função que junta todas as strings da lista numa só, separando-as por um espaço
unwords' :: [String] -> String
unwords' [] = ""
unwords' [s] = s 
unwords' (h:t) = h ++ " " ++ unwords' t

--33)definição recursiva da função que junta todas as strings da lista numa só, separando-as pelo caracter ’\n’
unlines' :: [String] -> String
unlines' [] = ""
unlines' (h:t) = h ++ "/n" ++ unlines' t

--34)definição recursiva da função que dada uma lista não vazia, retorna a posição onde se encontra o maior elemento da lista
pMaior :: Ord a => [a] -> Int
pMaior [x] = 0
pMaior (h:t) | h >= (t !! x) = 0
             | otherwise = 1 + x
    where x = pMaior t

--35)definição recursiva da função que retorna uma lista construída a partir de elementos de uma lista (o segundo argumento) atendendo a uma condição dada pelo primeiro argumento
lookup' :: Eq a => a -> [(a,b)] -> Maybe b
lookup' _ [] = Nothing
lookup' x ((a,b):t) | x == a = Just b
                    | otherwise = lookup' x t

--36)função calcula o maior prefixo crescente de uma lista
preCrescente :: Ord a => [a] -> [a]
preCrescente [] = []
preCrescente [x] = [x]
preCrescente (h:s:t) | s >= h = h : preCrescente (s:t)
                     | otherwise = [h]

--37)definição recursiva da função que calcula o resultado de ordenar uma lista. Podemos usar insert
iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (h:t) = insert' h (iSort t)

--38)definição recursiva da função que dadas duas strings, retorna True se e só se a primeira for menor do que a segunda, segundo a ordem do dicionário
menor :: String -> String -> Bool
menor _ "" = False
menor "" _ = True
menor (h:t) (c:s) | h == c = menor t s
                  | h < c = True
                  | otherwise = False

{-Usa-se o tipo [(a,Int)] para representar multi-conjuntos de elementos de a
  Nestas listas não há pares cuja primeira componente coincida, nem cuja segunda componente seja menor ou igual a zero

39)função que testa se um elemento pertence a um multi-conjunto-}
elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet _ [] = False
elemMSet e ((a,n):t) | e == a = True
                     | otherwise = elemMSet e t

--40)função que converte um multi-conjuto na lista dos seus elementos
converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((_,0):t) = converteMSet t
converteMSet ((a,n):t) = a : converteMSet ((a,n-1):t) 

--41)função que acrescenta um elemento a um multi-conjunto
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet e [] = [(e,1)] 
insereMSet e ((a,n):t) | e == a = ((a,n+1):t)
                       | otherwise = (a,n) : insereMSet e t

--42)função que remove um elemento a um multi-conjunto. Se o elemento não existir, devolve o original
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet e [] = []
removeMSet e ((a,n):t) | e == a = if n - 1 == 0 then t else (a,n-1):t
                       | otherwise = (a,n) : removeMSet e t

--43)função que dada uma lista ordenada por ordem crescente, calcula o multi-conjunto dos seus elementos
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (h:t) = constroiMSetAux h (constroiMSet t) -- resultado numa ordem diferente à do exemplo, para pôr igual podemos usar reverse ou last && init

constroiMSetAux :: Ord a => a -> [(a,Int)] -> [(a,Int)]
constroiMSetAux e [] = [(e,1)]
constroiMSetAux e ((a,n):t) | e == a = (a,n+1):t
                            | otherwise = (a,n) : constroiMSetAux e t

--44)definição recursiva da função que divide uma lista de Either's em duas listas   ??
partitionEithers' :: [Either a b] -> ([a],[b])
partitionEithers' [] = ([],[])
partitionEithers' ((Left a):t) = (a : as,bs)
    where (as,bs) = partitionEithers' t
partitionEithers' ((Right b):t) = (as,b : bs)
    where (as,bs) = partitionEithers' t

--45)definição recursiva da função que coleciona os elementos do tipo a de uma lista
catMaybes' :: [Maybe a] -> [a]
catMaybes' [] = []
catMaybes' (Just a:t) = a : catMaybes' t
catMaybes' (_:t) = catMaybes' t

--Tipo para representar movimentos de um robot
data Movimento = Norte | Sul | Este | Oeste
    deriving Show

--46)função que, dadas as posições inicial e final (coordenadas) do robot, produz uma lista de movimentos suficientes para que o robot passe de uma posição para a outra
caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (x,y) (xf,yf) | xf > x = Este : caminho (x+1,y) (xf,yf)
                      | xf < x = Oeste : caminho (x-1,y) (xf,yf)
                      | yf > y = Norte : caminho (x,y+1) (xf,yf)
                      | yf < y = Sul : caminho (x,y-1) (xf,yf)
                      | otherwise = []

--47)função que dada uma posição inicial e uma lista de movimentos (correspondentes a um percurso) verifica se o robot alguma vez volta a passar pela posição inicial ao longo do percurso
hasLoops :: (Int,Int) -> [Movimento] -> Bool
hasLoops _ [] = False
hasLoops p ms = p == posicao p ms || hasLoops p (init ms) 

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao p [] = p
posicao (x,y) (Norte:ms) = posicao (x, y + 1) ms
posicao (x,y) (Sul:ms) = posicao (x, y - 1) ms
posicao (x,y) (Este:ms) = posicao (x + 1, y) ms
posicao (x,y) (Oeste:ms) = posicao (x - 1, y) ms

-- tipos para representar pontos e retângulos.Os retângulos têm os lados paralelos aos eixos e são representados apenas por dois dos pontos mais afastados
type Ponto = (Float,Float)
data Rectangulo = Rect Ponto Ponto

--48)função que, dada uma lista com retângulos, conta quantos deles são quadrados
contaQuadrados :: [Rectangulo] -> Int
contaQuadrados [] = 0
contaQuadrados ((Rect (x,y) (a,b)):t) | abs(x-a)==abs(y-b) = 1 + contaQuadrados t
                                      | otherwise = contaQuadrados t

--49)função que, dada uma lista com retângulos, determina a ́area total que eles ocupam
areaTotal :: [Rectangulo] -> Float
areaTotal [] = 0
areaTotal ((Rect (x,y) (a,b)):t) = abs(a-x) * abs(b-y) + areaTotal t

--Tipo para representar o estado de um equipamento
data Equipamento = Bom | Razoavel | Avariado
    deriving Show

--50)função que determina a quantidade de equipamentos que não estão avariados
naoReparar :: [Equipamento] -> Int
naoReparar [] = 0
naoReparar (Avariado:t) = naoReparar t
naoReparar (_:t) = 1 + naoReparar t