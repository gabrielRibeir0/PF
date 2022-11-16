module Ficha3 where
import Ficha1

type Etapa = (Hora,Hora)
type Viagem = [Etapa]

--1)a)Testar se uma etapa está bem construída (Tchegada > Tpartida e horas válidas)
etapaValida :: Etapa -> Bool
etapaValida (ti,tf) = horaValida ti && horaValida tf && maiorHora tf ti

--b)Testa se uma viagem está bem construída (se as etapas são válidas e se a etapa seguinte começa depois da anterior)
viagemValida :: Viagem -> Bool
viagemValida [e] = etapaValida e
viagemValida ((ti,tf):(hi,hf):t) = (etapaValida (ti,tf) && maiorHora hi tf) && viagemValida ((hi,hf):t)

--c)Calcular a hora de partida e de chegada de uma dada viagem
partidaChegada :: Viagem -> (Hora,Hora)
partidaChegada v = (fst (head v), snd (last v))

--d)Dada uma viagem válida, calcular o tempo total de viagem efectiva
tempoViagem :: Viagem -> Hora
tempoViagem [] = H 0 0
tempoViagem ((ti,tf):t) = adicHora (difHora tf ti) (tempoViagem t)

--e)Calcular o tempo total de espera
tempoEspera :: Viagem -> Hora
tempoEspera [e] = H 0 0
tempoEspera ((ti,tf):(hi,hf):t) = adicHora (difHora hi tf) (tempoEspera ((hi,hf):t))

--f)Calcular o tempo total da viagem
tempoTotal :: Viagem -> Hora
tempoTotal v = adicHora (convToMin (tempoViagem v)) (tempoEspera v)
-- ou tempoTotal v = convToHora (difHora (fst (head v)) (snd (last v)))

--2)
type Poligonal = [Ponto] --linha poligonal

--a)função para calcular o comprimento de uma linha poligonal
compLinha :: Poligonal -> Double
compLinha [p] = 0
compLinha (p1:p2:l) = dist p1 p2 + compLinha (p2:l)

--b)função para testar se uma dada linha poligonal ́e ou não fechada
linhaFechada :: Poligonal -> Bool
linhaFechada [p] = False
linhaFechada [p1,p2] = False -- podemos usar length >= 3
linhaFechada l | head l == last l = True
               | otherwise = False 

--c)função que, dada uma linha poligonal fechada e convexa, calcule uma lista de triângulos cuja soma das áreas seja igual `a  ́area delimitada pela linha poligonal
triangula :: Poligonal -> [Figura]
triangula (p1:p2:p3:ps)
    | p1 == p3 = []
    | otherwise = Triangulo p1 p2 p3 : triangula (p1:p3:ps)
triangula _ = []

--d)função para calcular a área delimitada por uma linha poligonal fechada e convexa
areaPol :: Poligonal -> Double
areaPol p = areaTris (triangula p)

areaTris :: [Figura] -> Double
areaTris [] = 0
areaTris (h:t) = area h + areaTris t

--e)função que, dada uma linha poligonal e um ponto, devolve uma linha poligonal idêntica à primeira mas tendo como ponto inicial o ponto dado
mover :: Poligonal -> Ponto -> Poligonal
mover pol p = p : pol

--f)função que, dada um factor de escala e uma linha poligonal, devolve uma linha poligonal semelhante e com o mesmo ponto inicial mas em que o comprimento de cada segmento de recta é multiplicado pelo factor dado
zoom :: Double -> Poligonal -> Poligonal
zoom z (h:t) = mover (doZoom z h t) h

doZoom :: Double -> Ponto -> Poligonal -> Poligonal
doZoom z p [] = []
doZoom z p (h:t) = Cartesiano ((x - xp) * z + xp) ((y - yp) * z + yp) : doZoom z p t
    where x = posx h
          y = posy h
          xp = posx p
          yp = posy p

--3)tipo de dados para amazenar uma agenda de contatos telefónicos e de emails
data Contacto = Casa Integer
              | Trab Integer
              | Tlm Integer
              | Email String
              deriving Show
type Nome = String
type Agenda = [(Nome, [Contacto])]

--a)função que, dado um nome, um email e uma agenda, acrescenta essa informação à agenda
acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail nome e [] = [(nome, [Email e])]
acrescEmail nome e ((n,cs):t)
    | nome == n = (n, Email e : cs) : t
    | otherwise = (n,cs) : acrescEmail nome e t

--b)função que, dado um nome e uma agenda, retorna a lista dos emails associados a esse nome. Se não existir na agenda retorna Nothing
verEmails :: Nome -> Agenda -> Maybe [String]
verEmails _ [] = Nothing
verEmails nome ((n,cs):t) | nome == n = Just (getEmail cs)
                          | otherwise = verEmails nome t

getEmail :: [Contacto] -> [String]
getEmail [] = []
getEmail (Email e : cs) = e : getEmail cs
getEmail (_:cs) = getEmail cs

--c)função que, dada uma lista de contactos, retorna a lista de todos os números de telefone dessa lista
consTelefs :: [Contacto] -> [Integer]
consTelefs [] = []
consTelefs (c:cs) = case c of
                    Email _ -> consTelefs cs
                    Casa n -> n: consTelefs cs
                    Tlm n -> n: consTelefs cs
                    Trab n -> n: consTelefs cs

--d)função que, dado um nome e uma agenda, retorna o número de telefone de casa (caso exista)
casa :: Nome -> Agenda -> Maybe Integer
casa _ [] = Nothing
casa nome ((n,cs):t) | nome == n = getCasa cs
                     | otherwise = casa nome t

getCasa :: [Contacto] -> Maybe Integer
getCasa [] = Nothing
getCasa (Casa n:cs) = Just n
getCasa (_:cs) = getCasa cs

--4)tipo de dados para guardar datas de aniversários numa tabela
type Dia = Int
type Mes = Int
type Ano = Int
--type Nome = String
data Date = Da Dia Mes Ano
    deriving Show
type TabDN = [(Nome,Date)]

--a)função que indica a Date de nascimento de uma dada pessoa, caso o seu nome exista na tabela
procura :: Nome -> TabDN -> Maybe Date
procura _ [] = Nothing
procura nome ((n,d):t) | nome == n = Just d
                       | otherwise = procura nome t

--b) função que calcula a idade de uma pessoa numa dada Date
idade :: Date -> Nome -> TabDN -> Maybe Int
idade _ _ [] = Nothing
idade (Da dx mx ax) nome ((n,Da d m a):ts) 
    | nome == n = Just (calculaIdade (Da d m a) (Da dx mx ax))
    | otherwise = idade (Da dx mx ax) nome ts

calculaIdade :: Date -> Date -> Int
calculaIdade (Da dn mn an) (Da d m a) = if m > mn || m == mn && d > dn then a - an else a - an - 1

--c)função que testa se uma Date é anterior a outra Date
anterior :: Date -> Date -> Bool
anterior (Da d m a) (Da d2 m2 a2) = a < a2 || (a == a2 && (m < m2 || (m == m2 && d < d2)))

--d)função que ordena uma tabela de Dates de nascimento, por ordem crescente das Dates de nascimento
ordena :: TabDN -> TabDN
ordena [] = []
ordena ((n,d):ts) = insereDN (n,d) (ordena ts)

insereDN :: (Nome,Date) -> TabDN -> TabDN
insereDN (n,d) [] = [(n,d)]
insereDN (n,d) ((nh,dh):t) | anterior d dh = (n,d) : (nh,dh) : t
                           | otherwise = (nh,dh) : insereDN (n,d) t

--e)função que apresenta o nome e a idade das pessoas, numa dada Date, por ordem crescente da idade das pessoas
porIdade :: Date -> TabDN -> [(Nome,Int)]
porIdade d tabela = porIdadeAux d (ordena tabela)

porIdadeAux :: Date -> TabDN -> [(Nome,Int)]
porIdadeAux _ [] = []
porIdadeAux d ((nh,dh):t) = porIdadeAux d t ++ [(nh, calculaIdade dh d)]

--5)tipos de dados para representar um extrato bancário
data Movimento = Credito Float | Debito Float
    deriving Show
data Data = D Int Int Int
    deriving Show
data Extracto = Ext Float [(Data, String, Movimento)]
    deriving Show

--a)função que produz uma lista de todos os movimentos (créditos ou débitos) superiores a um determinado valor
extValor :: Extracto -> Float -> [Movimento]
extValor (Ext si ((_,_,mov):t)) valor = if getValor mov > valor then mov : extValor (Ext si t) valor else extValor (Ext si t) valor

getValor :: Movimento -> Float
getValor (Credito x) = x
getValor (Debito x) = x

--b)função que retorna informação relativa apenas aos movimentos cuja descrição esteja incluída na lista
filtro :: Extracto -> [String] -> [(Data,Movimento)]
filtro (Ext _ []) _ = []
filtro (Ext si ((dat,desc,mov):t)) listaStr = if desc `elem` listaStr then (dat,mov) : filtro (Ext si t) listaStr else filtro (Ext si t) listaStr

--c)função que retorna o total de créditos e de débitos de um extracto no primeiro e segundo elementos de um par, respectivamente
creDeb :: Extracto -> (Float,Float)
creDeb (Ext _ []) = (0,0)
creDeb (Ext si ((_,_,Credito x):t)) = (x + cr, dr)
    where (cr,dr) = creDeb (Ext si t)
creDeb (Ext si ((_,_,Debito x):t)) = (cr, x + dr)
    where (cr,dr) = creDeb (Ext si t)

creDeb' :: Extracto -> (Float,Float)
creDeb' (Ext _ []) = (0,0)
creDeb' (Ext si ((_,_,mov):t)) = (c + cr, d + dr)
    where (cr,dr) = creDeb (Ext si t)
          (c,d) = case mov of Credito x -> (x,0)
                              Debito x -> (0,x)

--d)função que devolve o saldo final que resulta da execução de todos os movimentos no extracto sobre o saldo inicial
saldo :: Extracto -> Float
saldo (Ext si []) = si
saldo (Ext si ((_,_,Debito x):t)) = saldo (Ext (si + x) t)
saldo (Ext si ((_,_,Credito x):t)) = saldo (Ext (si - x) t)