module Ficha8 where
--1)tipo de dados para representar frações
data Frac = F Integer Integer

--a)fração que dada uma fração, calcula uma equivalente, irredutível e com o denominador positivo
normaliza :: Frac -> Frac

mdc :: Integer -> Integer -> Integer

--b)definir Frac como instância da classe Eq
instance Eq Frac where
    f1 == f2 = (a==x) && (b == y)
        where (F a b) = normaliza f1
              (F x y) = normaliza f2

--c)definir Frac como instância da classe Ord

--d)definir Frac como instância da classe Show, de forma a ser apresentada por (num/denom)
instance Show Frac where 
    show :: Frac -> String
    show (F a b) = "(" ++ (show a) ++ "/" ++ (show b)++
--e)definir Frac como instância da classe Num
instance Num Frac where
    (F a b) + (F x y) = F (a*y + b*x) (b*y)
    (F a b) * (F x y) = F (a*x) (b*y)
    negate (F a b) = F (-a) b
    abs (F a b) = F (abs a) (abs b)
    
