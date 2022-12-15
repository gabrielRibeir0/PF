module Main where

import LI
import T1
import T2
import T3
import T4
import T5
import Data.List
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

data ModoJogo = MenuJogar   --opção 'jogar' selecionada no menu
                | MenuSair  --opção 'sair' selecionada no menu (add mais?)
                | Jogar     --enquanto joga
                | Perdeu     --quando o jogo termina
                | Pausa MenuPausa --menu de pausa (tipo o menu inicial, mas com opções de voltar ou sair)

data MenuPausa = VoltarJogo | Sair

type PlayerMove = Jogada

type Estado = (ModoJogo, Jogo)

gerarMapaInicial :: [Int]  --lista de números aleatórios
                 -> Mapa   --mapa gerado (no início vazio)
                 -> Int    --número de iterações da função (número de linhas do mapa inicial)
                 -> Mapa
gerarMapaInicial _ m 0 = m
gerarMapaInicial randList m n = gerarMapaInicial (init randList) (estendeMapa m (randList !! (n-1))) (n-1)

mainDisplay :: Display
mainDisplay = InWindow "Crossy Road" (1280,640) (0,0)

estadoInicial :: Mapa -> Estado
estadoInicial mapaInicial = (MenuJogar, Jogo (Jogador (2,4)) mapaInicial)

drawOption :: String -> Picture
drawOption option = Translate (-50) 0 $ Scale (0.5) (0.5) $ Text option

arvore :: Picture
arvore = color green (ThickCircle 0 20)

carro :: Picture 
carro = color red $ polygon [(0,0),(40,0),(40,40),(0,40)]

tronco :: Picture
tronco = color black $ polygon [(0,0),(40,0),(40,40),(0,40)]

rio :: Picture
rio = color blue $ polygon [(0,0),(250,0),(250,50),(0,50)]

estrada :: Picture
estrada = color (greyN 0.75) $ polygon [(0,0),(250,0),(250,50),(0,50)]

relva :: Picture
relva = color green $ polygon [(0,0),(250,0),(250,50),(0,50)]

jogador :: Picture
jogador = color green (ThickCircle 0 40)

drawState :: Estado -> Picture
drawState (MenuJogar, jogo) = Pictures [Color blue $ drawOption "Jogar", Translate 0 (-70) $ drawOption "Sair"]
drawState (MenuSair, jogo) = Pictures [drawOption "Jogar", Color blue $ Translate 0 (-70) $ drawOption "Sair"]
drawState (Pausa VoltarJogo, jogo) = Pictures [Color blue $ drawOption "Voltar ao Jogo", Translate 0 (-70) $ drawOption "Sair"]
drawState (Pausa Sair, jogo) = Pictures [drawOption "Voltar ao Jogo", Color blue $ Translate 0 (-70) $ drawOption "Sair"]
drawState (Jogar, Jogo (Jogador (x,y)) (Mapa _ ll)) = Pictures (drawMap (0,0) ll ++ [Translate i j jogador])
  where i = fromIntegral x
        j = fromIntegral y

drawMap :: (Int, Int) -> [(Terreno,[Obstaculo])] -> [Picture]
drawMap _  [] = []
drawMap (x,y) ((t,os):ts) = case t of 
  Relva -> translate 0 (fromIntegral(y*(-50))) (Pictures (relva : drawLine x (t,os))) : drawMap (x,y+1) ts
  Rio _ -> translate 0 (fromIntegral(y*(-50))) (Pictures (rio : drawLine x (t,os))) : drawMap (x,y+1) ts
  Estrada _ -> translate 0 (fromIntegral(y*(-50))) (Pictures (estrada : drawLine x (t,os))) : drawMap (x,y+1) ts

drawLine :: Int -> (Terreno,[Obstaculo]) -> [Picture]
drawLine _ (_,[]) = []
drawLine x (Relva,o:os) = case o of 
  Nenhum -> translate (fromIntegral(x*50)) 0 Blank : drawLine (x+1) (Relva,os)
  Arvore -> translate (fromIntegral(x*50)) 0 arvore : drawLine (x+1) (Relva,os)
drawLine x (Rio v,o:os) = case o of 
  Nenhum -> translate (fromIntegral(x*50)) 0 Blank : drawLine (x+1) (Rio v,os)
  Tronco -> translate (fromIntegral(x*50)) 0 tronco : drawLine (x+1) (Rio v,os)
drawLine x (Estrada v,o:os) = case o of 
  Nenhum -> translate (fromIntegral(x*50)) 0 Blank : drawLine (x+1) (Estrada v,os)
  Carro -> translate (fromIntegral(x*50)) 0 carro : drawLine (x+1) (Estrada v,os)

event :: Event -> Estado -> Estado
--menu inicial
event (EventKey (SpecialKey KeyUp) Down _ _) (MenuJogar, jogo) = (MenuSair, jogo)
event (EventKey (SpecialKey KeyDown) Down _ _) (MenuJogar, jogo) = (MenuSair, jogo)
event (EventKey (SpecialKey KeyEnter) Down _ _) (MenuJogar, jogo) = (Jogar, jogo)
event (EventKey (SpecialKey KeyUp) Down _ _) (MenuSair, jogo) = (MenuJogar, jogo)
event (EventKey (SpecialKey KeyDown) Down _ _) (MenuSair, jogo) = (MenuJogar, jogo)
event (EventKey (SpecialKey KeyEnter) Down _ _) (MenuSair, jogo) = error "Fim de Jogo"

--movimentos no jogo
event (EventKey (SpecialKey KeyEsc) Down _ _) (Jogar, jogo) = (Pausa VoltarJogo, jogo)
event (EventKey (SpecialKey key) Down _ _) (Jogar, Jogo (Jogador c) (Mapa l ll)) = case key of
  KeyUp -> if jogoTerminou (Jogo (Jogador (moveJogador c l ll (Move Cima))) (Mapa l ll)) then (Perdeu, Jogo (Jogador (c)) (Mapa l ll)) else (Jogar, Jogo (Jogador (moveJogador c l ll (Move Cima))) (Mapa l ll))
  KeyDown -> if jogoTerminou (Jogo (Jogador (moveJogador c l ll (Move Baixo))) (Mapa l ll)) then (Perdeu, Jogo (Jogador (c)) (Mapa l ll)) else (Jogar, Jogo (Jogador (moveJogador c l ll (Move Baixo))) (Mapa l ll))
  KeyRight -> if jogoTerminou (Jogo (Jogador (moveJogador c l ll (Move Direita))) (Mapa l ll)) then (Perdeu, Jogo (Jogador (c)) (Mapa l ll)) else (Jogar, Jogo (Jogador (moveJogador c l ll (Move Direita))) (Mapa l ll))
  KeyLeft -> if jogoTerminou (Jogo (Jogador (moveJogador c l ll (Move Esquerda))) (Mapa l ll)) then (Perdeu, Jogo (Jogador (c)) (Mapa l ll)) else (Jogar, Jogo (Jogador (moveJogador c l ll (Move Esquerda))) (Mapa l ll))

--menu de pausa
event (EventKey (SpecialKey KeyEsc) Down _ _) (Pausa VoltarJogo, jogo) = (Jogar, jogo)
event (EventKey (SpecialKey KeyEsc) Down _ _) (Pausa Sair, jogo) = (Jogar, jogo)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Pausa VoltarJogo, jogo) = (Jogar, jogo)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Pausa Sair, jogo) = error "Fim de Jogo"
event _ e = e

--time ->
--mover obstaculos com as velocidades (?)
--deslizar o mapa (?)
--score
time :: Float -> Estado -> Estado --a passagem do tempo é a movimentação dos obstáculos
time t (Jogar, jogo) = (Jogar, animaJogo jogo Parado)
time t (m, jogo) = (m, jogo)

main :: IO ()
main = do 
  n <- randomRIO (0,100)
  let randList = take 5 $ randoms (mkStdGen n)                 --
  let mapaInicial = gerarMapaInicial randList (Mapa 5 []) 5    --onde se define a largura e o número de linhas que o mapa tem
  play mainDisplay
       (greyN 0.25)      --background color
       15                --fps
       (estadoInicial mapaInicial)
       drawState
       event 
       time