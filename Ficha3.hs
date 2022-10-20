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

--c)função triangula :: Poligonal -> [Figura] que, dada uma linha poligonal fechada e convexa, calcule uma lista de triângulos cuja soma das áreas seja igual `a  ́area delimitada pela linha poligonal