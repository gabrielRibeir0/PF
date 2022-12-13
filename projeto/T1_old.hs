{- |
Module      : Tarefa1_2022li1g067
Description : Validação de um mapa
Copyright   : José Luís Matos Ribeiro  <a105108@alunos.uminho.pt>
              Gabriel Pereira Ribeiro  <a104171@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2022/23.
-}
module T1_old where
import LI
import Data.List

mapaValido :: Mapa -> Bool
mapaValido (Mapa l ll) = terrenoValido ll && riosOpostos ll && tamanhoObstaculo ll && larguraIgual ll l && terrenosSeguidos ll

{-restrição 1 verificar se não existem obstáculos incompatíveis com o terreno
+ restrição 5 para verificar se todas as linhas têm um nenhum
+ restrição extra para verificar que os rios têm pelo menos um tronco-}
terrenoValido :: [(Terreno, [Obstaculo])] -> Bool
terrenoValido [] = True
terrenoValido ((Rio v, os): ts) | elem Arvore os || elem Carro os || not (elem Nenhum os) || not (elem Tronco os) = False
                                | otherwise = terrenoValido ts
terrenoValido ((Estrada v, os): ts) | elem Arvore os || elem Tronco os || not (elem Nenhum os) = False
                                    | otherwise = terrenoValido ts
terrenoValido ((Relva, os): ts) | elem Tronco os || elem Carro os || not (elem Nenhum os) = False
                                | otherwise = terrenoValido ts

--restrição 2 verificar se rios seguidos têm sentidos opostos (caso encontre 2 rios seguidos que v1*v2 > 0 retorna logo False)
riosOpostos :: [(Terreno, [Obstaculo])] -> Bool
riosOpostos [] = True
riosOpostos ((Rio v1, o1):(Rio v2, o2):t) | v1 * v2 >= 0 = False
                                          | otherwise = riosOpostos ((Rio v2, o2): t)
riosOpostos (_:(Rio v, o):t) = riosOpostos ((Rio v, o):t)
riosOpostos (_:_:t) = riosOpostos t

--restrição 3 e 4 verificar se os troncos têm no máximo 5 u.c e os carros no máximo 3 u.c - tendo em conta os obstáculos divididos no loop
tamanhoObstaculo :: [(Terreno,[Obstaculo])] -> Bool
tamanhoObstaculo [] = True
tamanhoObstaculo ((Relva, os):ts) = tamanhoObstaculo ts
tamanhoObstaculo ((t, os):ts) | medidorObstaculos (group os) && loopObstaculo (group os) = tamanhoObstaculo ts
                              | otherwise = False
        where loopObstaculo :: [[Obstaculo]] -> Bool
              loopObstaculo ((Carro:t):os) | head(last os) == Carro = not (length ((Carro:t)++(last os)) > 3)
                                           | otherwise = True
              loopObstaculo ((Tronco:t):os) | head(last os) == Tronco = not (length ((Tronco:t)++(last os)) > 5)
                                            | otherwise = True
              loopObstaculo ((_:t):os) = True

medidorObstaculos :: [[Obstaculo]] -> Bool
medidorObstaculos [] = True
medidorObstaculos ((Carro:t):os) | length t > 3 = False 
                                 | otherwise = medidorObstaculos os
medidorObstaculos ((Tronco:t):os) | length t > 5 = False 
                                  | otherwise = medidorObstaculos os

--restrição 6 verificar se a lista de obstáculos tem tamanho igual à largura
larguraIgual :: [(Terreno,[Obstaculo])] -> Largura -> Bool
larguraIgual [] _ = True
larguraIgual ((_,os):t) l | length os == l = larguraIgual t l
                          | otherwise = False

--restrição 7 não podem existir mais do que 4 rios ou 5 estradas/relvas seguidas
terrenosSeguidos :: [(Terreno,[Obstaculo])] -> Bool 
terrenosSeguidos ll =  medidaTerrenos( group( listaTerrenos ll))

--função para criar uma lista apenas com o tipo de terreno
listaTerrenos :: [(Terreno,[Obstaculo])] -> [Terreno]
listaTerrenos [] = [] 
listaTerrenos ((Rio _, _):ts) = Rio 0 : listaTerrenos ts
listaTerrenos ((Estrada _, _):ts) = Estrada 0 : listaTerrenos  ts
listaTerrenos ((Relva, _):ts) = Relva : listaTerrenos ts

--função para ver se há alguma sequência de terrenos maior que o permitido
medidaTerrenos :: [[Terreno]] -> Bool 
medidaTerrenos [] = True 
medidaTerrenos ((Rio v:t):ts) | length t > 3 = False 
                              | otherwise = medidaTerrenos ts
medidaTerrenos ((_:t):ts) | length t > 4 = False
                          | otherwise = medidaTerrenos ts