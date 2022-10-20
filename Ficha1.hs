module Ficha1 where 
import Data.Char (ord, chr)
--1) a) calcula o perı́metro de uma circunferência, dado o comprimento do seu raio
perimetro1 :: Double -> Double
perimetro1 r = 2 * pi * r

--b) calcula a distância entre dois pontos no plano Cartesiano. Cada ponto é um par de valores do tipo Double
dist1 :: (Double, Double) -> (Double, Double) -> Double
dist1 (x1 , y1) (x2 , y2) = sqrt((x1 - x2)^2 + (y1 - y2)^2)

--c) recebe uma lista e devolve um par com o primeiro e o último elemento dessa lista
primUlt :: [b] -> (b, b)
primUlt l = (head l, last l)

--d) testa se o número inteiro m é múltiplo de n
multiplo :: Int -> Int -> Bool
multiplo m n = mod m n == 0

--e) recebe uma lista e, se o comprimento da lista for ı́mpar retira-lhe o primeiro elemento, caso contrário devolve a própria lista
truncaImpar :: [a] -> [a]
truncaImpar l = if mod (length l) 2 == 0 then l else tail l 
truncaImparb :: [a] -> [a]
truncaImparb l | mod (length l) 2 == 0 = l
               | otherwise             = tail l 

--f) calcula o maior de dois números inteiros
max2 :: Int -> Int -> Int
max2 a b = if a > b then a else b

--g) calcula o maior de três números inteiros, usando a função max2
max3 :: Int -> Int -> Int -> Int
max3 a b c = max2 a (max2 b c)
max3b :: Ord a => a -> a -> a -> a
max3b a b c | a > b = if a > c then a else c 
            | b > c = b 
            | otherwise = c


--2) a) função que recebe os (3) coeficientes de um polinómio de 2o grau e que calcula o número de raı́zes (reais) desse polinómio
nRaizes :: Double -> Double -> Double -> Int
nRaizes a b c | b^2 - 4*a*c == 0 = 1 
              | b^2 - 4*a*c > 0  = 2 
              | otherwise        = 0

-- b) função que, usando a função anterior, recebe os coeficientes do polinómio e calcula a lista das suas raı́zes reais
raizes :: Double -> Double -> Double -> [Double]
raizes a b c | nRaizes a b c == 2 = [((-b) - sqrt(b^2 - 4*a*c))/(2*a) , ((-b) + sqrt(b^2 - 4*a*c))/(2*a)]
             | nRaizes a b c == 1 = [((-b) + sqrt(b^2 - 4*a*c))/(2*a)]
             | otherwise = []


--3)
type Horab = (Int,Int)   --representar horas por um par de números inteiros

--a) testar se um par de inteiros representa uma hora do dia válida
horaValida' :: Horab -> Bool
horaValida' (h,m) | 0 <= h && h < 24 && 0 <= m && m < 60 = True
                 | otherwise = False

--b) testar se uma hora é ou não depois de outra (comparação)
compHora' :: Horab -> Horab -> Bool
compHora' (h,m) (x,y) | h > x || (h == x && m > y) = True
                     | otherwise = False

--c) converter um valor em horas (par de inteiros) para minutos (inteiro)
convHora' :: Horab -> Int
convHora' (h,m) = h * 60 + m

--d) converter um valor em minutos para horas
convMin' :: Int -> Horab
convMin' n = (div n 60,mod n 60)

--e) calcular a diferença entre duas horas (cujo resultado deve ser o número de minutos)
difHora' :: Horab -> Horab -> Int
difHora' (h,m) (x,y) = (h - x) * 60 + m - y

--f) adicionar um determinado número de minutos a uma dada hora
adicHora' :: Int -> Horab -> Horab
adicHora' n (h,m) = convMin' (convHora' (h,m) + n)


--4) igual a 3), mas as horas são representadas por um novo tipo de dados
data Hora = H Int Int deriving (Show,Eq)

--a)
horaValida :: Hora -> Bool
horaValida (H h m) | h >= 0 && h < 24 && m >= 0 && m < 60 = True
                    | otherwise = False

--b)
maiorHora :: Hora -> Hora -> Bool
maiorHora (H h m) (H x y) | h > x || (h == x && m > y) = True
                          | otherwise = False

--c)
convToMin :: Hora -> Int
convToMin (H h m) = h * 60 + m

--d)
convToHora :: Int -> Hora
convToHora n = H (div n 60) (mod n 60)

--e)
difHora :: Hora -> Hora -> Int
difHora (H h m) (H x y) = (h - x) * 60 + m - y

--f)
adicHora :: Int -> Hora -> Hora
adicHora n (H h m) = convToHora (convToMin (H h m) + n)


--5)
data Semaforo = Verde | Amarelo | Vermelho deriving (Show, Eq)

--a) função que calcula o próximo estado de um semáforo
next :: Semaforo -> Semaforo
next Verde = Amarelo
next Amarelo = Vermelho
next Vermelho = Verde

--b) função que determona se é obrigatório parar num semáforo
stop :: Semaforo -> Bool
stop Vermelho = True
stop _ = False

--c) função que testa se o estado de dois semáforos num cruzamento é seguro
safe :: Semaforo -> Semaforo -> Bool
safe a b = a == Vermelho || b == Vermelho

--6) um ponto pode ser representado -Cartesiano (dist aos eixos v e h) -Polar (dist à orig e ang do vetor ao eixo h)
data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)

--a) função que calcula a distância de um ponto ao eixo vertical // dá a abcissa
posx :: Ponto -> Double
posx (Cartesiano x y) = x
posx (Polar r a) =  r * cos a

--b) função que calcula a distância de um ponto ao eixo horizontal // dá a ordenada
posy :: Ponto -> Double
posy (Cartesiano x y) = y
posy (Polar r a) =  r * sin a

--c) função que calcula a distância de um ponto à origem
raio :: Ponto -> Double
raio (Cartesiano x y) = sqrt (x^2 + y^2)
--raio (Polar r a) = sqrt ((r * cos a)^2 + (r * sin a)^2)
raio (Polar r a) = r

--d) função que calcula o ângulo entre o vector que liga a origem ao ponto e o eixo horizontal
angulo :: Ponto -> Double 
angulo (Cartesiano x y) | y == 0 && x < 0 = pi  
                        | otherwise = atan(y/x)
angulo (Polar r a) = a

--e) função que calcula a distância entre dois pontos
dist :: Ponto -> Ponto -> Double
dist a b = sqrt((posx b - posx a)^2 + (posy b - posy a)^2)

--7)
data Figura = Circulo Ponto Double | Retangulo Ponto Ponto | Triangulo Ponto Ponto Ponto deriving (Show,Eq)

--a) função que teste se uma figura é um polígono
poligono :: Figura -> Bool 
poligono (Circulo _ _) = False
poligono (Retangulo p1 p2) = posx p1 /= posx p2 && posy p1 /= posy p2
poligono (Triangulo p1 p2 p3) = posx p1 /= posx p2 ||
                                posx p2 /= posx p3 ||
                                posx p1 /= posx p3
                                &&
                                posy p1 /= posy p2 ||
                                posy p2 /= posy p3 ||
                                posy p1 /= posy p3

--b) função que que calcula a lista dos vértices de uma figura
vertices :: Figura -> [Ponto]
vertices (Circulo _ _) = []
vertices (Retangulo a b) = [a, b, Cartesiano (posx a) (posy b), Cartesiano (posx b) (posy a)] 
vertices (Triangulo a b c) = [a,b,c]

--c) Completar a seguinte definição cujo objetivo é calcular a área de uma figura
area :: Figura -> Double
area (Triangulo p1 p2 p3) =
    let a = dist p1 p2
        b = dist p2 p3
        c = dist p3 p1
        s = (a+b+c) / 2           -- semi-perimetro
    in sqrt (s*(s-a)*(s-b)*(s-c)) -- formula de Heron
area (Retangulo p1 p2) = abs(posx p1 - posx p2) * abs(posy p1 - posy p2)
area (Circulo c r) = pi * r^2

--d) função que calcula o perímetro de uma figura
perimetro :: Figura -> Double
perimetro (Circulo c r) = 2 * pi * r
perimetro (Triangulo a b c) = dist a b + dist b c + dist c a
perimetro (Retangulo a b) = 2 * (abs(posx a - posx b) + abs(posy a - posy b))


{-8) chr - recebe um código ASCII e devolve o Char correspondente
     ord - recebe um Char e devolve o seu código ASCII
Usando estas funções definir as seguintes:-}

--a) função que testa se um Char é uma minúscula
isLower :: Char -> Bool
isLower c | ord c >= ord 'a' && ord c <= ord 'z' = True  
          | otherwise = False

--b) função que testa se um Char é um dígito
isDigit :: Char -> Bool
isDigit c | ord c >= 48 && ord c <= 57 = True
          | otherwise = False
    
--c) função que testa se um Char é uma letra
isAlpha :: Char -> Bool
isAlpha c | (ord c >= 65 && ord c <= 90) || (ord c >= 97 && ord c <= 122) = True
          | otherwise = False

--d) função que converte uma letra para a respetiva maiúscula
toUpper :: Char -> Char
toUpper l | ord l >= 65 && ord l <= 90 = l
          | ord l >= 97 && ord l <= 122 = chr (ord l - 32)
          | otherwise = error "Não é uma letra"

--e) função que converte um número entre 0 e 9 para o respetivo dígito
intToDigit :: Int -> Char
intToDigit n | n >= 0 && n <= 9 = chr (ord '0' + n)
             | otherwise = error "Não é um número entre 0 e 9"

--f) função que converte um dígito para o respetivo inteiro
digitToInt :: Char -> Int
digitToInt c | isDigit c = ord c - 48
             | otherwise = error "Não é um número"