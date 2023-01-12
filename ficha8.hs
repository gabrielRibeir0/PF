module Ficha8 where
import Data.List
import Data.Char

--1)tipo de dados para representar frações
data Frac = F Integer Integer

--a)fração que dada uma fração, calcula uma equivalente, irredutível e com o denominador positivo
normaliza :: Frac -> Frac
normaliza (F a b)
    | b < 0 = normaliza $ F (-a) (-b)
    | otherwise = 
        let d = mdc a b in
        F (a `div` d) (b `div` d)

mdc :: Integer -> Integer -> Integer
mdc x 0 = x
mdc 0 y = y
mdc x y = mdc y (x `mod` y)

--b)definir Frac como instância da classe Eq
instance Eq Frac where
    (==) :: Frac -> Frac -> Bool
    f1 == f2 = a1 == a2 && b1 == b2
        where F a1 b1 = normaliza f1
              F a2 b2 = normaliza f2

--c)definir Frac como instância da classe Ord
instance Ord Frac where
    (<=) :: Frac -> Frac -> Bool
    f1 <= f2 = a1 * b2 <= a2 * b1
        where F a1 b1 = normaliza f1
              F a2 b2 = normaliza f2

--d)definir Frac como instância da classe Show, de forma a ser apresentada por (num/denom)
instance Show Frac where
    show :: Frac -> String
    show f = show a ++ "/" ++ show b
        where F a b = normaliza f

--e)definir Frac como instância da classe Num
instance Num Frac where
    (+) :: Frac -> Frac -> Frac
    (F a b) + (F c d) = normaliza $ F (a * d + b * c) (b * d)
    
    (-) :: Frac -> Frac -> Frac
    x - y = x + negate y

    (*) :: Frac -> Frac -> Frac
    (F a b) * (F c d) = normaliza $ F (a * c) (b * d)
    
    negate :: Frac -> Frac
    negate (F a b) = normaliza $ F (-a) b
    
    abs :: Frac -> Frac
    abs f = F (abs a) b
        where F a b = normaliza f
    
    signum :: Frac -> Frac
    signum f = F (signum a) 1
        where F a b = normaliza f
    
    fromInteger :: Integer -> Frac
    fromInteger x = F x 1

--f)Função que, dada uma fração f e uma lista de frações l, selecciona de l os elementos que são maiores do que o dobro de f
maioresQueDobro :: Frac -> [Frac] -> [Frac]
maioresQueDobro = filter . (<) . (2 *)

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
    deriving Eq
data Extracto = Ext Float [(Data, String, Movimento)]

--a)Definir Data como instância da classe Ord
instance Ord Data where
    compare :: Data -> Data -> Ordering
    compare (D dia1 mes1 ano1) (D dia2 mes2 ano2) 
        | ano1 > ano2 || ano1 == ano2 && (mes1 > mes2 || mes1 == mes2 && dia1 > dia2) = GT
        | ano1 == ano2 && mes1 == mes2 && dia1 == dia2 = EQ
        | otherwise = LT

--b)Definir Data como instância da classe Show
instance Show Data where 
    show :: Data -> String
    show (D dia mes ano) = intercalate "/" $ map show [ano,mes,dia]

--c)função que transforma um extrato que modo a que a lista de movimentos apareça pro ordem crescente de data
ordena :: Extracto -> Extracto
ordena (Ext n l) = Ext n (sortBy (\(data1,_,_) (data2,_,_) -> compare data1 data2) l)

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
instance Show Extracto where
    show :: Extracto -> String
    show ext = "Saldo anterior: " ++ show n ++
               "\n---------------------------------------" ++
               "\nData       Descricao" ++ replicate (desc_max - 9) ' ' ++ "Credito" ++ replicate (cred_max - 7) ' ' ++ "Debito" ++
               "\n---------------------------------------\n" ++
               unlines (map (\(dat,desc,mov) -> 
                    show dat ++ replicate (data_max - length (show dat)) ' ' 
                    ++ map toUpper desc ++ replicate (desc_max - length desc) ' ' 
                    ++ case mov of Credito quant -> show quant ++ replicate (cred_max - length (show quant)) ' '; Debito _ -> replicate cred_max ' '
                    ++ case mov of Debito quant -> show quant; Credito _ -> ""
               ) movs) ++
               "---------------------------------------" ++
               "\nSaldo actual: " ++ show (saldo ext)
        where (Ext n movs) = ordena ext
              data_max = 11
              desc_max = max (length "Descricao   ") (maximum $ map (\(_,desc,_) -> length desc) movs)
              cred_max = max (length "Credito   ") (maximum $ map (\(_,_,mov) -> case mov of Credito x -> length (show x); _ -> 0) movs)

saldo :: Extracto -> Float
saldo (Ext x lm) = foldl (\acc (_,_,mov) -> case mov of Credito n -> acc + n
                                                        Debito n -> acc - n) x lm