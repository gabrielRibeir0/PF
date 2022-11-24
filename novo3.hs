
module Tarefa3_2022li1g067 where
import LI

animaJogo :: Jogo -> Jogada -> Jogo
animaJogo (Jogo (Jogador (x,y)) (Mapa l ll)) j = Jogo (moveJogador (x,y) l ll j) (Mapa l (moveObstaculos ll))

moveJogador :: Coordenadas -> Largura -> [(Terreno,[Obstaculo])] -> Jogada -> Jogador
moveJogador (x,y) l ll (Move Cima) | snd(ll !! y) !! x == Tronco = if y < 1 || snd(ll !! (y-1)) !! x == Arvore then posJogador 0 (x,y) (ll !! y) else Jogador (x,y-1)
                                   | otherwise = if y < 1 || snd(ll !! (y-1)) !! x == Arvore then Jogador (x,y) else Jogador (x, y-1)
moveJogador (x,y) l ll (Move Baixo) | snd(ll !! y) !! x == Tronco = if y+1 >= length ll || snd(ll !! (y+1)) !! x == Arvore then posJogador 0 (x,y) (ll !! y) else Jogador (x,y+1)
                                    | otherwise = if y+1 >= length ll || snd(ll !! (y+1)) !! x == Arvore then Jogador (x,y) else Jogador (x, y+1)
moveJogador (x,y) l ll (Move Esquerda) | not (x < 1) && snd(ll !! y) !! (x-1) == Tronco = posJogador (-1) (x,y) (ll !! y)
                                       | otherwise = if x < 1 || snd(ll !! y) !! (x-1) == Arvore then Jogador (x, y) else Jogador (x-1, y)
moveJogador (x,y) l ll (Move Direita) | not (x+1 >= l) && snd(ll !! y) !! (x+1) == Tronco = posJogador 1 (x,y) (ll !! y)
                                      | otherwise = if x+1 >= l || snd(ll !! y) !! (x+1) == Arvore then Jogador (x, y) else Jogador (x+1, y)
moveJogador (x,y) _ ll Parado | snd(ll !! y) !! x == Tronco = posJogador 0 (x,y) (ll !! y)
                              | otherwise = Jogador (x,y)

posJogador :: Int -> Coordenadas -> (Terreno, [Obstaculo]) -> Jogador
posJogador 0 (x,y) (Rio v,_) = Jogador (x+v,y)
posJogador m (x,y) (Rio v,_) = Jogador (x+v+m,y)

--função para ver quantas casas o carro pode andar até atropelar o jogador
--mover os obstáculos como até agora, mas no máximo o número de casas de cima. Caso seja menor é o número normal 
maxCasas :: (Terreno,[Obstaculo]) -> Jogador -> Int
maxCasas (Estrada 0, os) _ = 0
maxCasas (Estrada v, os) (Jogador (x,y))| debaixoCarro (x,y) (Estrada v,os) = 0
                                        | otherwise = if v > 0 then 1 + maxCasas (Estrada (v-1), (last os : init os)) (Jogador (x,y))
                                                      else 1 + maxCasas (Estrada (v+1), (tail os ++ [head os])) (Jogador (x,y))
maxCasas _ _ = 100

debaixoCarro :: Coordenadas -> (Terreno,[Obstaculo]) -> Bool
debaixoCarro (x,y) (Estrada v, os) | os !! x == Carro = True
                                   | otherwise = False
debaixoCarro _ _ = False


moveObstaculos :: [(Terreno,[Obstaculo])] -> [(Terreno,[Obstaculo])] 
moveObstaculos [] = [] 
moveObstaculos ((Relva, os):ts) = (Relva,os) : moveObstaculos ts
moveObstaculos ((Rio v, os):ts) | v > 0 = (Rio v, moveDireita os v) : moveObstaculos ts
                                | otherwise = (Rio v, moveEsquerda os (abs v)) : moveObstaculos ts
moveObstaculos ((Estrada v, os):ts) | v > 0 = (Estrada v, moveDireita os v) : moveObstaculos ts
                                    | otherwise = (Estrada v, moveEsquerda os (abs v)) : moveObstaculos ts


moveDireita :: [Obstaculo] -> Int -> [Obstaculo]
moveDireita l 0 = l
moveDireita l n = moveDireita (last l : init l) (n -1)

moveEsquerda :: [Obstaculo] -> Int -> [Obstaculo]
moveEsquerda l 0 = l
moveEsquerda l n = moveEsquerda (tail l ++ [head l] ) (n - 1)