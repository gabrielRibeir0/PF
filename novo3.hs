module Tarefa3_2022li1g067 where
import LI
n = Nenhum
mapa = Mapa 6 [(Estrada 3, [Carro, n, Carro, n, n, n])]
jogador = Jogador (3, 0)
jogo = Jogo jogador mapa

animaJogo :: Jogo -> Jogada -> Jogo
animaJogo (Jogo (Jogador (x,y)) (Mapa l ll)) j = Jogo (Jogador (newPos)) (Mapa l (moveObstaculos ll (maxCasas (ll !! snd(newPos)) newPos) ))
    where newPos = moveJogador (x,y) l ll j

moveJogador :: Coordenadas -> Largura -> [(Terreno,[Obstaculo])] -> Jogada -> Coordenadas
moveJogador (x,y) l ll (Move Cima) | snd(ll !! y) !! x == Tronco = if y < 1 || snd(ll !! (y-1)) !! x == Arvore then posJogador 0 (x,y) (ll !! y) else (x,y-1)
                                   | otherwise = if y < 1 || snd(ll !! (y-1)) !! x == Arvore then (x,y) else (x, y-1)
moveJogador (x,y) l ll (Move Baixo) | snd(ll !! y) !! x == Tronco = if y+1 >= length ll || snd(ll !! (y+1)) !! x == Arvore then posJogador 0 (x,y) (ll !! y) else (x,y+1)
                                    | otherwise = if y+1 >= length ll || snd(ll !! (y+1)) !! x == Arvore then (x,y) else (x, y+1)
moveJogador (x,y) l ll (Move Esquerda) | not (x < 1) && snd(ll !! y) !! (x-1) == Tronco = posJogador (-1) (x,y) (ll !! y)
                                       | otherwise = if x < 1 || snd(ll !! y) !! (x-1) == Arvore then (x, y) else (x-1, y)
moveJogador (x,y) l ll (Move Direita) | not (x+1 >= l) && snd(ll !! y) !! (x+1) == Tronco = posJogador 1 (x,y) (ll !! y)
                                      | otherwise = if x+1 >= l || snd(ll !! y) !! (x+1) == Arvore then (x, y) else (x+1, y)
moveJogador (x,y) _ ll Parado | snd(ll !! y) !! x == Tronco = posJogador 0 (x,y) (ll !! y)
                              | otherwise = (x,y)

posJogador :: Int -> Coordenadas -> (Terreno, [Obstaculo]) -> Coordenadas
posJogador 0 (x,y) (Rio v,_) = (x+v,y)
posJogador m (x,y) (Rio v,_) = (x+v+m,y)

--função para ver quantas casas o carro pode andar até atropelar o jogador
--mover os obstáculos como até agora, mas no máximo o número de casas de cima. Caso seja menor é o número normal 
maxCasas :: (Terreno,[Obstaculo]) -> Coordenadas -> Int
maxCasas (Estrada 0, _) _ = 0
maxCasas (Estrada v, os) (x,y) | os !! x == Carro = 0
                               | otherwise = if v > 0 then 1 + maxCasas (Estrada (v-1), last os : init os) (x,y) 
                                             else 1 + maxCasas (Estrada (v+1), tail os ++ [head os]) (x,y)
maxCasas _ _ = 100

moveObstaculos :: [(Terreno,[Obstaculo])] -> Int -> [(Terreno,[Obstaculo])] 
moveObstaculos [] _ = [] 
moveObstaculos ((Relva, os):ts) max = (Relva,os) : moveObstaculos ts max
moveObstaculos ((Rio v, os):ts) max | v > max && v > 0 = (Rio v, moveDireita os max) : moveObstaculos ts max 
                                    | v <= max && v >= 0 = (Rio v, moveDireita os v) : moveObstaculos ts max 
                                    | abs v > max && v < 0 = (Rio v, moveEsquerda os max) : moveObstaculos ts max 
                                    | abs v <= max && v <= 0 = (Rio v, moveEsquerda os (abs v)) : moveObstaculos ts max
moveObstaculos ((Estrada v, os):ts) max | v > max && v > 0 = (Estrada v, moveDireita os max) : moveObstaculos ts max 
                                        | v <= max && v >= 0 = (Estrada v, moveDireita os v) : moveObstaculos ts max 
                                        | abs v > max && v < 0 = (Estrada v, moveEsquerda os max) : moveObstaculos ts max 
                                        | abs v <= max && v <= 0 = (Estrada v, moveEsquerda os (abs v)) : moveObstaculos ts max

moveDireita :: [Obstaculo] -> Int -> [Obstaculo]
moveDireita l 0 = l
moveDireita l n = moveDireita (last l : init l) (n -1)

moveEsquerda :: [Obstaculo] -> Int -> [Obstaculo]
moveEsquerda l 0 = l
moveEsquerda l n = moveEsquerda (tail l ++ [head l] ) (n - 1)