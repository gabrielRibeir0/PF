module T5 where
import LI
import T2

deslizaJogo :: Int -> Jogo -> Jogo
deslizaJogo i (Jogo(Jogador (x,y)) (Mapa l ll)) = Jogo (Jogador (x,y+1)) (estendeMapa (Mapa l (init ll)) i)