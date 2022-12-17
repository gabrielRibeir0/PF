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

type Estado = (ModoJogo, Jogo)

gerarMapaInicial :: [Int]  --lista de números aleatórios
                 -> Mapa   --mapa gerado (no início vazio)
                 -> Int    --número de iterações da função (número de linhas do mapa inicial)
                 -> Mapa
gerarMapaInicial _ m 0 = m
gerarMapaInicial randList m n = gerarMapaInicial (init randList) (estendeMapa m (randList !! (n-1))) (n-1)

mainDisplay :: Display
mainDisplay = InWindow "Crossy Road" (640,1000) (0,0)

--mapa de exemplo
exmap :: Mapa
exmap = Mapa 8 [(Relva, [Nenhum, Arvore, Nenhum, Nenhum, Arvore, Nenhum, Nenhum, Arvore])
            ,(Relva, [Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum])
            ,(Estrada 1, [Nenhum, Carro, Carro, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum])
            ,(Estrada 1, [Nenhum, Carro, Nenhum, Nenhum, Nenhum, Carro, Carro, Carro])
            ,(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Nenhum, Nenhum, Nenhum, Nenhum])
            ,(Relva, [Arvore, Arvore, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Arvore])
            ,(Relva, [Arvore, Nenhum, Arvore, Nenhum, Nenhum, Nenhum, Arvore, Arvore])
            ,(Rio 2, [Tronco, Tronco, Nenhum, Tronco, Tronco, Nenhum, Nenhum, Nenhum])
            ,(Rio (-1), [Nenhum, Tronco, Tronco, Tronco, Nenhum, Tronco, Tronco, Tronco])
            ,(Rio 1, [Tronco, Nenhum, Nenhum, Tronco, Nenhum, Nenhum, Tronco, Tronco])
            ]

estadoInicial :: Mapa -> Estado
estadoInicial mapaInicial = (MenuJogar, Jogo (Jogador (2,0)) exmap)

--estadoInicial mapaInicial = (MenuJogar, Jogo (Jogador (2,0)) mapaInicial)

--função dos exemplos para escrever os textos das opções
drawOption :: String -> Picture
drawOption option = Translate (-50) 0 $ Scale (0.5) (0.5) $ Text option

--elementos do mapa (depois é para substituir por imagens)
arvore :: Picture
arvore = translate 40 50 (color orange (circleSolid 40))

carro :: Picture 
carro = translate 40 50 (color red (circleSolid 40))

tronco :: Picture
tronco = translate 40 50 (color black (circleSolid 40))

rio :: Picture
rio = color blue $ polygon [(0,0),(640,0),(640,100),(0,100)]

estrada :: Picture
estrada = color (greyN 0.75) $ polygon [(0,0),(640,0),(640,100),(0,100)]

relva :: Picture
relva = color green $ polygon [(0,0),(640,0),(640,100),(0,100)]

jogador :: Picture
jogador = translate 40 50 (color yellow (circleSolid 40))
--------

drawState :: Estado -> Picture
drawState (MenuJogar, jogo) = Pictures [Color blue $ drawOption "Jogar", Translate 0 (-70) $ drawOption "Sair"]
drawState (MenuSair, jogo) = Pictures [drawOption "Jogar", Color blue $ Translate 0 (-70) $ drawOption "Sair"]
drawState (Pausa VoltarJogo, jogo) = Pictures [Color blue $ drawOption "Voltar ao Jogo", Translate 0 (-70) $ drawOption "Sair"]
drawState (Pausa Sair, jogo) = Pictures [drawOption "Voltar ao Jogo", Color blue $ Translate 0 (-70) $ drawOption "Sair"]
drawState (Jogar, Jogo (Jogador (x,y)) (Mapa _ ll)) = Pictures (drawLines (0,0) ll ++ [Translate i j jogador])
  where i = fromIntegral (-240 + x*80)
        j = fromIntegral (400 - (y*100))

--desenhar cada linha e chamar a função para os obstáculos
drawLines :: (Int, Int) -> [(Terreno,[Obstaculo])] -> [Picture]
drawLines _  [] = []
drawLines (x,y) ((t,os):ts) = case t of 
  Relva -> translate (-320) (fromIntegral(400 - (y*100))) (Pictures (relva : drawObstacles x (t,os))) : drawLines (x,y+1) ts
  Rio _ -> translate (-320) (fromIntegral(400 - (y*100))) (Pictures (rio : drawObstacles x (t,os))) : drawLines (x,y+1) ts
  Estrada _ -> translate (-320) (fromIntegral(400 - (y*100))) (Pictures (estrada : drawObstacles x (t,os))) : drawLines (x,y+1) ts

--desenhar os obstáculos (chamada a cada linha)
drawObstacles :: Int -> (Terreno,[Obstaculo]) -> [Picture]
drawObstacles _ (_,[]) = []
drawObstacles x (Relva,o:os) = case o of 
  Nenhum -> translate (fromIntegral(x*80)) 0 Blank : drawObstacles (x+1) (Relva,os)
  Arvore -> translate (fromIntegral(x*80)) 0 arvore : drawObstacles (x+1) (Relva,os)
drawObstacles x (Rio v,o:os) = case o of 
  Nenhum -> translate (fromIntegral(x*80)) 0 Blank : drawObstacles (x+1) (Rio v,os)
  Tronco -> translate (fromIntegral(x*80)) 0 tronco : drawObstacles (x+1) (Rio v,os)
drawObstacles x (Estrada v,o:os) = case o of 
  Nenhum -> translate (fromIntegral(x*80)) 0 Blank : drawObstacles (x+1) (Estrada v,os)
  Carro -> translate (fromIntegral(x*80)) 0 carro : drawObstacles (x+1) (Estrada v,os)

event :: Event -> Estado -> Estado
--menu inicial
event (EventKey (SpecialKey KeyUp) Down _ _) (MenuJogar, jogo) = (MenuSair, jogo)
event (EventKey (SpecialKey KeyDown) Down _ _) (MenuJogar, jogo) = (MenuSair, jogo)
event (EventKey (SpecialKey KeyEnter) Down _ _) (MenuJogar, jogo) = (Jogar, jogo)
event (EventKey (SpecialKey KeyUp) Down _ _) (MenuSair, jogo) = (MenuJogar, jogo)
event (EventKey (SpecialKey KeyDown) Down _ _) (MenuSair, jogo) = (MenuJogar, jogo)
event (EventKey (SpecialKey KeyEnter) Down _ _) (MenuSair, jogo) = error "Fim de Jogo"

--movimentos no jogo   (jogoTerminou aqui ?)
event (EventKey (Char 'q') Down _ _) (Jogar, jogo) = (Pausa VoltarJogo, jogo)
event (EventKey (SpecialKey key) Down _ _) (Jogar, Jogo (Jogador c) (Mapa l ll)) = case key of
  KeyUp -> (Jogar, Jogo (Jogador (moveJogador c l ll (Move Cima))) (Mapa l ll))
  KeyDown -> (Jogar, Jogo (Jogador (moveJogador c l ll (Move Baixo))) (Mapa l ll))
  KeyRight -> (Jogar, Jogo (Jogador (moveJogador c l ll (Move Direita))) (Mapa l ll))
  KeyLeft -> (Jogar, Jogo (Jogador (moveJogador c l ll (Move Esquerda))) (Mapa l ll))
  _ -> (Jogar, Jogo (Jogador c) (Mapa l ll))

--menu de pausa
event (EventKey (SpecialKey KeyUp) Down _ _) (Pausa VoltarJogo, jogo) = (Pausa Sair, jogo)
event (EventKey (SpecialKey KeyDown) Down _ _) (Pausa VoltarJogo, jogo) = (Pausa Sair, jogo)
event (EventKey (SpecialKey KeyUp) Down _ _) (Pausa Sair, jogo) = (Pausa VoltarJogo, jogo)
event (EventKey (SpecialKey KeyDown) Down _ _) (Pausa Sair, jogo) = (Pausa VoltarJogo, jogo)
event (EventKey (Char 'q') Down _ _) (Pausa VoltarJogo, jogo) = (Jogar, jogo)
event (EventKey (Char 'q') Down _ _) (Pausa Sair, jogo) = (Jogar, jogo)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Pausa VoltarJogo, jogo) = (Jogar, jogo)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Pausa Sair, jogo) = error "Fim de Jogo"
event _ e = e

--time ->
--mover obstaculos com as velocidades (?)
--deslizar o mapa (?)
--score
time :: Float -> Estado -> Estado --a passagem do tempo é a movimentação dos obstáculos
--time t (Jogar, jogo) = (Jogar, animaJogo jogo Parado)
time t (m, jogo) = (m, jogo)

main :: IO ()
main = do 
  n <- randomRIO (0,100)
  let randList = take 10 $ randoms (mkStdGen n)                 --
  let mapaInicial = gerarMapaInicial randList (Mapa 8 []) 10    --onde se define a largura e o número de linhas que o mapa tem
  play mainDisplay
       (greyN 0.25)      --background color
       15                --fps
       (estadoInicial mapaInicial)
       drawState
       event 
       time