module Ficha2 where
import Data.Char ( digitToInt, isDigit, isLower )
{-1) Indicar como +e que o interpretador de Haskell avalia as expressões das alíneas que se
seguem, apresentando os vários passos intermédios até ao valor final de cada uma dessas expressões-}

--a) Diga, justificando, qual é o valor de funA [2,3,5,1]
funA :: [Double] -> Double
funA [] = 0
funA (y:ys) = y^2 + funA ys
{-funA [2,3,5,1] 
                = 2^2 + funA [3,5,1] 
                = 4 + 3^2 + funA [5,1] 
                = 4 + 9 + 5^2 + funA [1] 
                = 4 + 9 + 25 + 1^2 + funA [] 
                = 4 + 9 + 25 + 1 + 0 
                = 39.0 -}
                  
--b)Diga, justificando, qual  ́e o valor de funB [8,5,12]
funB :: [Int] -> [Int]
funB [] = []
funB (h:t) = if mod h 2 ==0 then h : (funB t)
                             else (funB t)
{-funB [8,5,12]                         mod 8 2 == 0
                = 8 : funB [5,12]       mod 5 2 /= 0
                = 8 : funB [12]         mod 12 2 /= 0
                = 8 : 12 : funB []
                = 8 : 12 : []
                = [8,12] -}


--c)Diga, justificando, qual ́e o valor de funC [1,2,3,4,5]
funC (x:y:t) = funC t
funC [x] = [x]
funC [] = []
{-funC [1,2,3,4,5] 
                  = funC [3,4,5]
                  = funC [5]
                  = [5] -}

--d)Diga, justificando, qual  ́e o valor de funD "otrec"
funD :: [a] -> [a]
funD l = g [] l
g :: [a] -> [a] -> [a]
g acc [] = acc
g acc (h:t) = g (h:acc) t
{-funD "otrec"
              = g [] "otrec"
              = g ('o':[]) "trec"
              = g ('t':['o']) "rec"
              = g ('r':"to") "ec"
              = g ('e':"rto") "c"
              = g ('c':"erto") ""
              = "certo" -}

--2) Defina recursivamente as seguintes funções sobre listas
--a) recebe uma lista e produz a lista em que cada elemento é o dobro do valor correspondente na lista de entrada
dobros :: [Float] -> [Float]
dobros [] = []
dobros (h:t) = h*2 : dobros t

--b) calcula o número de vezes que um caracter ocorre numa string
numOcorre :: Char -> String -> Int
numOcorre c [] = 0
numOcorre c (h:t) | c == h = 1 + numOcorre c t
                  | otherwise = numOcorre c t

--c)testa se uma lista só tem elementos positivos
positivos :: [Int] -> Bool
--positivos [] = True
positivos [x] = x > 0
positivos (x:xs) | x > 0 = positivos xs
                 | otherwise = False

--d)retira todos os elementos não positivos de uma lista de inteiros
soPos :: [Int] -> [Int]
soPos [] = []
soPos (h:t) | h > 0 = h : soPos t
            | otherwise = soPos t

--e)soma todos os números negativos da lista de entrada
somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (h:t) | h < 0 = h + somaNeg t
              | otherwise = somaNeg t

--f)devolve os ́ultimos três elementos de uma lista. Se a lista de entrada tiver menos de três elementos, devolve a própria lista
tresUlt :: [a] -> [a]
tresUlt [] = []
tresUlt (h:t) | length (h:t) <= 3 = (h:t)
              | otherwise = tresUlt t

tresUlt' (a:b:c:[]) = [a,b,c]
tresUlt' (a:b:c:xs) = tresUlt (b:c:xs)

--g)calcula a lista das segundas componentes dos pares
segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((_,y):t) = y : segundos t

--h)testa se um elemento aparece na lista como primeira componente de algum dos pares
nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros _ [] = False
nosPrimeiros a ((x,_):t) | a == x = True
                         | otherwise = nosPrimeiros a t

--i)soma uma lista de triplos componente a componente
sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos ((x,y,z):t) = let (a,b,c) = sumTriplos t         --                             = (x + a, y + b, z + c)  
                         in (x + a, y + b, z + c)           -- ou podemos usar where ->      where (a,b,c) = sumTriplos t

--3)usando funções de Data.Char, definir recursivamente as funções
--a)recebe uma lista de caracteres, e selecciona dessa lista os caracteres que são algarismos
soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (h:t) | isDigit h = h : soDigitos t
                | otherwise = soDigitos t

--b)recebe uma lista de caracteres, e conta quantos desses caracteres são letras minúsculas
minusculas :: [Char] -> Int
minusculas [] = 0
minusculas (h:t) | isLower h = 1 + minusculas t
                 | otherwise = minusculas t

--c)recebe uma string e devolve uma lista com os algarismos que ocorrem nessa string, pela mesma ordem
nums :: String -> [Int]
nums [] = []
nums (h:t) | isDigit h = digitToInt h : nums t
           | otherwise = nums t

--4)representar polinómios de uma variável é usar listas de monómios representados por pares (coeficiente, expoente)
type Polinomio = [Monomio]
type Monomio = (Float,Int)

--a)(conta n p) indica quantos monómios de grau n existem em p
conta :: Int -> Polinomio -> Int
conta _ [] = 0
conta n ((c,e):t) | n == e = 1 + conta n t
                  | otherwise = conta n t

--b)indica o grau de um polinómio
grau :: Polinomio -> Int
grau [(x,y)] = y
grau ((_,e):t)  | e > grau t = e
                | otherwise = grau t

--c)selecciona os monómios com um dado grau de um polinómio
selgrau :: Int -> Polinomio -> Polinomio
selgrau _ [] = []
selgrau g ((c,e):t) | g == e = (c,e) : selgrau g t
                    | otherwise = selgrau g t

--d)calcula a derivada de um polinómio
deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((c,e):t) | e == 0 = deriv t 
                | otherwise = (c * fromIntegral e, e - 1) : deriv t -- fromIntegral converte um Int em Num para poder fazer Float * Int

--e)calcula o valor de um polinómio para uma dado valor de x
calcula :: Float -> Polinomio -> Float
calcula _ [] = 0
calcula x ((c,e):t) = c * x^e + calcula x t

--f)retira de um polinómio os monómios de coeficiente zero
simp :: Polinomio -> Polinomio
simp [] = []
simp ((c,e):t) | c == 0 = simp t
               | otherwise = (c,e) : simp t

--g)calcula o resultado da muliplicação de um monómio por um polinómio
mult :: Monomio -> Polinomio -> Polinomio
mult _ [] = []
mult (cm,em) ((c,e):t) = (c * cm, e + em) : mult (cm,em) t

--h)dado um polinómio constrói um polinómio equivalente em que não podem aparecer varios monómios com o mesmo grau
normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza ((c,e):t) = constroi (c,e) (normaliza t)

constroi :: Monomio -> Polinomio -> Polinomio
constroi m [] = [m]
constroi (c,e) ((c1,e1):t) | e == e1 = (c + c1, e) : t
                           | otherwise = (c1,e1) : constroi (c,e) t

--i)soma dois polinómios deforma a que se os polinómios que recebe estiverem normalizados produz também um polinómio normalizado
soma :: Polinomio -> Polinomio -> Polinomio
soma p [] = p
soma [] p = p
soma ((c,g):t) p = constroi (c,g) (soma t p)

--j)calcula o produto de dois polinómios
produto :: Polinomio -> Polinomio -> Polinomio
produto [] _ = []
produto (m:t) p = mult m p ++ produto t p

--k)ordena um polinómio por ordem crescente dos graus dos seus monómios
ordena :: Polinomio -> Polinomio
ordena [] = []
ordena (h:t) = insert h (ordena t)

insert :: Monomio -> Polinomio -> Polinomio
insert m [] = [m]
insert (cm,em) ((c,e):t) | em <= e = (cm,em):(c,e):t 
                         | otherwise = (c,e) : insert (cm,em) t

--l)testa se dois polinómios são equivalentes
equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2= ordena (normaliza p1) == ordena (normaliza p2)
