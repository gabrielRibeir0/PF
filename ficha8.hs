module Ficha8 where
--1)tipo de dados para representar frações
data Frac = F Integer Integer

--a)fração que dada uma fração, calcula uma equivalente, irredutível e com o denominador positivo
--normaliza :: Frac -> Frac

--mdc :: Integer -> Integer -> Integer

--b)definir Frac como instância da classe Eq
instance Eq Frac where
--    f1 == f2 = (a==x) && (b == y)
--        where (F a b) = normaliza f1
--              (F x y) = normaliza f2

--c)definir Frac como instância da classe Ord

--d)definir Frac como instância da classe Show, de forma a ser apresentada por (num/denom)
instance Show Frac where 
    show :: Frac -> String
    show (F a b) = "(" ++ (show a) ++ "/" ++ (show b)
--e)definir Frac como instância da classe Num
instance Num Frac where
    (F a b) + (F x y) = F (a*y + b*x) (b*y)
    (F a b) * (F x y) = F (a*x) (b*y)
    negate (F a b) = F (-a) b
    abs (F a b) = F (abs a) (abs b)

--2)tipo de dados para representar expressoes inteiras
data Exp a = Const a
           | Simetrico (Exp a)
           | Mais (Exp a) (Exp a)
           | Menos (Exp a) (Exp a)
           | Mult (Exp a) (Exp a)

e :: Exp Int
e = Mais (Const 2) (Mult (Const 3) (Const 4)) --2+3*4 = 14

e2 :: Exp Int
e2 = Mais (Mult (Const 3) (Const 4)) (Const 2)

--a)Declarar Exp a como uma instância de Show
toString :: Show a => Exp a -> String
toString (Const i) = show i
toString (Simetrico e) = "-(" ++ toString e ++ ")"
toString (Mais e1 e2) = '(' : toString e1 ++ " + " ++ toString e2 ++ ")"
toString (Menos e1 e2) = '(' : toString e1 ++ " - " ++ toString e2 ++ ")"
toString (Mult e1 e2) = '(' : toString e1 ++ " * " ++ toString e2 ++ ")"

instance Show a => Show (Exp a) where
    show = toString
    
--b)Declarar Exp a como uma instância de Eq
calcula :: Num a => Exp a -> a
calcula (Const i) = i
calcula (Simetrico e) = -(calcula e)
calcula (Mais e1 e2) = calcula e1 + calcula e2
calcula (Menos e1 e2) = calcula e1 - calcula e2
calcula (Mult e1 e2) = calcula e1 * calcula e2

iguaisExp :: (Eq a, Num a) => Exp a -> Exp a -> Bool
iguaisExp e1 e2 = calcula e1 == calcula e2

instance (Eq a, Num a) => Eq (Exp a) where
    (==) = iguaisExp

--c)Declarar Exp a como uma instância de Num
somaExp :: Num a => Exp a -> Exp a -> Exp a
somaExp e1 e2 = Const (calcula e1 + calcula e2)

subExp :: Num a => Exp a -> Exp a -> Exp a
subExp e1 e2 = Const (calcula e1 - calcula e2)

multExp :: Num a => Exp a -> Exp a -> Exp a
multExp e1 e2 = Const (calcula e1 * calcula e2)

absExp :: Num a => Exp a -> Exp a
absExp e = Const (abs(calcula e))

sigNumExp :: Num a => Exp a -> Exp a
sigNumExp e = Const(signum (calcula e))

fromIntegerExp :: Num a => Integer -> Exp a
fromIntegerExp i = Const (fromInteger i)

instance Num a => Num (Exp a) where
    (+) = somaExp
    (-) = subExp
    (*) = multExp
    abs = absExp
    signum = sigNumExp
    fromInteger = fromIntegerExp

--(extra) d)Declare Exp a como instância da classe Ord
menorIgualExp :: (Ord a, Num a) => Exp a -> Exp a -> Bool
menorIgualExp e1 e2 = calcula e1 <= calcula e2

instance (Ord a, Num a) => Ord (Exp a) where
    (<=) = menorIgualExp

--3)tipo de dadoos para representar um extrato bancário
data Movimento = Credito Float | Debito Float
data Data = D Int Int Int
data Extracto = Ext Float [(Data, String, Movimento)]

--a)Definir Data como instância da classe Ord

--b)Definir Data como instância da classe Show

--c)função que transforma um extrato que modo a que a lista de movimentos apareça pro ordem crescente de data

--d)Definir Extracto como instância da classe Show, de forma a que a apresentação do extrato seja por ordem de data do movimento com o seguinte e com o aspeto:
{-
Saldo anterior: 300
---------------------------------------
Data Descricao Credito Debito
---------------------------------------
2010/4/5 DEPOSITO 2000
2010/8/10 COMPRA 37,5
2010/9/1 LEV 60
2011/1/7 JUROS 100
2011/1/22 ANUIDADE 8
---------------------------------------
Saldo actual: 2294,5
-}
