{- |
Module      : Tarefa4_2022li1g067
Description : Determinar se o jogo terminou
Copyright   : José Luís Matos Ribeiro  <a105108@alunos.uminho.pt>
              Gabriel Pereira Ribeiro  <a104171@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2022/23.
-}
module T4 where

import LI

jogoTerminou :: Jogo -> Bool
jogoTerminou (Jogo (Jogador (x,y)) (Mapa l ll)) = x < 0 || x >= l || y < 0 || y >= length ll || caiuNaAgua (x,y) (ll !! y) || debaixoCarro (x,y) (ll !! y)

caiuNaAgua :: Coordenadas -> (Terreno,[Obstaculo]) -> Bool
caiuNaAgua (x,y) (Rio v, os) | os !! x == Nenhum = True
                             | otherwise = False
caiuNaAgua _ _ = False

debaixoCarro :: Coordenadas -> (Terreno,[Obstaculo]) -> Bool
debaixoCarro (x,y) (Estrada v, os) | os !! x == Carro = True
                                   | otherwise = False
debaixoCarro _ _ = False