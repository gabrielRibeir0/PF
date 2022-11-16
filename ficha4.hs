module Ficha4 where
import Data.Char
import Data.Fixed (divMod')
--1)função, que dada uma string, devolve um par de strings: um apenas com as letras, outra com os números. Implementar de modo a fazer apenas uma travessia pela string.
digitAlpha :: String -> (String,String)
digitAlpha "" = ("","")
digitAlpha (c:cs) | isDigit c = (l,c:n)
                  | isAlpha c = (c:l,n)
                  | otherwise = (l,n)
    where (l,n) = digitAlpha cs

--2)função que, dada uma lista de inteiros,conta o número de valores nagativos, o número de zeros e o número de valores positivos,num triplo.Percorre a lista só uma vez
nzp :: [Int] -> (Int,Int,Int)
nzp [] = (0,0,0)
nzp (h:t) | h > 0 = (n,z,1+p)
          | h < 0 = (1+n,z,p)
          | otherwise = (n,1+z,p)
    where (n,z,p) = nzp t

--nzp V2 com acumulador
nzpAc :: [Int] -> (Int,Int,Int)
nzpAc l = nzpAc' (0,0,0) l

nzpAc' :: (Int,Int,Int) -> [Int] -> (Int,Int,Int)
nzpAc' ac [] = ac
nzpAc' (n,z,p) (h:t) | h > 0 = nzpAc' (n,z,1+p) t
                     | h < 0 = nzpAc' (1+n,z,p) t
                     | otherwise = nzpAc' (n,1+z,p) t

--Ex soma dos int de uma lista, versão normal e depois versão com acumuladores
somatorio :: [Int] -> Int
somatorio [] = 0
somatorio (h:t) = h + somatorio t

somatorioAc :: [Int] -> Int
somatorioAc l = somatorioAc' 0 l
    where somatorioAc' :: Int -> [Int] -> Int
          somatorioAc' ac [] = ac
          somatorioAc' ac (h:t) = somatorioAc' (ac+h) t

--Ex 1 digitAlpha com acumuladores
digitAlphaAc :: String -> (String,String)
digitAlphaAc s = digitAlphaAc' ([],[]) s

digitAlphaAc' :: (String,String) -> String -> (String,String)
digitAlphaAc' ac "" = ac
digitAlphaAc' (l,n) (c:cs) | isDigit c = digitAlphaAc' (l,c:n) cs
                           | isAlpha c = digitAlphaAc' (c:l,n) cs
                           | otherwise = digitAlphaAc' (l,n) cs

--3)função que calcula simultaneamente a divisão e o resto da divisão inteira por subtrações sucessivas
divModv2:: Integral a => a -> a -> (a, a)
divModv2 a b | a < b = (0,a)
             | a == b = (1,0)
             | otherwise = (d+1,r)
    where (d,r) = divModv2 (a-b) b

--4)função auxiliar com um acumulador, optimizar a definição recursiva que determina qual o número que corresponde a uma lista de digitos
fromDigits :: [Int] -> Int
fromDigits [] = 0
fromDigits (h:t) = h*10^(length t) + fromDigits t

fromDigitsAc :: [Int] -> Int 
fromDigitsAc l = fromDigitsAc' 0 l

fromDigitsAc' :: Int -> [Int] -> Int
fromDigitsAc' ac [] = ac
fromDigitsAc' ac (h:t) = fromDigitsAc' (h+ac*10) t

--5)função auxiliar com acumuladores, optimize a definição que determina a soma do segmento inicial de uma lista com soma máxima
{-maxSumInit :: (Num a, Ord a) => [a] -> a
maxSumInit l = maximum [sum m | m <- inits l]-}
maxSumInit :: (Num a, Ord a) => [a] -> a
maxSumInit l = maxSumInitAux l (sum l)

maxSumInitAux :: (Num a, Ord a) => [a] -> a -> a
maxSumInitAux [] acc = acc
maxSumInitAux l acc = if si > acc then maxSumInitAux il si else maxSumInitAux il acc
    where il = init l
          si = sum il

--6)função auxiliar com acumuladores para otimizar a função que calcula o n-ésimo número da seq. de Fibonacci
{-fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)-}
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fibAux (n-2) 0 1

fibAux :: Int -> Int -> Int -> Int
fibAux 0 n _ = n
fibAux i fib_n fib_n_mais_1 = fibAux (i - 1) fib_n_mais_1 (fib_n + fib_n_mais_1)

--7)função auxiliar com aucumuladores para a função que converte um inteiro numa string
intToStr :: Integer -> String
intToStr 0 = "zero"
intToStr n = intToStrAux n ""

intToStrAux :: Integer -> String -> String
intToStrAux 0 ('-':acc) = acc
intToStrAux n acc = intToStrAux nn ((case r of 
        0 -> "-zero"
        1 -> "-um"
        2 -> "-dois"
        3 -> "-três"
        4 -> "-quatro"
        5 -> "-cinco"
        6 -> "-seis"
        7 -> "-sete"
        8 -> "-oito"
        9 -> "-nove") ++ acc)
    where (nn,r) = n `divMod` 10 

--8)Enumerar as listas seguintes e apresentar uma forma alternativa de as obter
--a)[x | x <- [1..20], mod x 2 == 0, mod x 3 == 0]
l8a = [6,12,18]
l8a2 = [x | x <- [1..20], mod x 6 == 0]

--b)[x | x <- [y | y <- [1..20], mod y 2 == 0], mod x 3 == 0]
l8b = [6,12,18]
l8b2 = [x | x <- [1..20], mod x 6 == 0]

--c)[(x,y) | x <- [0..20], y <- [0..20], x+y == 30]
l8c = [(10,20),(11,19),(12,18),(13,17),(14,16),(15,15),(16,14),(17,13),(18,12),(19,11),(20,10)]
l8c2 = [(x, 30 - x) | x <- [10..20]]

--d)[sum [y | y <- [1..x], odd y] | x <- [1..10]]
l8d = [1,1,4,4,9,9,16,16,25,25]
l8d2 = [ x^2 | x <- [1..5], y <- [1..2]]

--9)definir as listas por compreensão
--a) [1,2,4,8,16,32,64,128,256,512,1024]
e9a = [2^x | x <- [0..10]]

--b)[(1,5),(2,4),(3,3),(4,2),(5,1)]
e9b = [(x,y) | x <- [1..5], y <- [1..5] , x+y == 6]
e9b' = [(x,6-x)|x <- [1..5]]

--c)[[1],[1,2],[1,2,3],[1,2,3,4],[1,2,3,4,5]]
e9c = [[1..x]| x<-[1..5]]

--d)[[1],[1,1],[1,1,1],[1,1,1,1],[1,1,1,1,1]]
e9d = [replicate x 1 | x<- [1..5]]
e9d' = [take n (repeat 1) | n <- [1..5]]

--e)[1,2,6,24,120,720]
e9e = [product [y | y <- [1..x]] | x <- [1..6]]