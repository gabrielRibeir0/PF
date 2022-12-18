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
                | MenuCreditos
                | MenuSair  --opção 'sair' selecionada no menu 
                | Jogar     --enquanto joga
                | Creditos
                | Perdeu MenuPerdeu     --quando o jogo termina
                | Pausa MenuPausa --menu de pausa (tipo o menu inicial, mas com opções de voltar ou sair)
              deriving Eq

data MenuPausa = VoltarJogo | GuardarSair | SairPausa 
  deriving Eq

data MenuPerdeu = JogarDeNovo | PerdeuSair
  deriving Eq

type Score = Float
type Estado = (ModoJogo, Jogo, Score)

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
            ,(Estrada 2, [Carro, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum])
            ,(Estrada 1, [Carro, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum])
            ,(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Nenhum, Nenhum, Nenhum, Nenhum])
            ,(Relva, [Arvore, Arvore, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Arvore])
            ,(Relva, [Arvore, Nenhum, Arvore, Nenhum, Nenhum, Nenhum, Arvore, Arvore])
            ,(Rio 2, [Tronco, Tronco, Nenhum, Tronco, Tronco, Nenhum, Nenhum, Nenhum])
            ,(Rio (-1), [Nenhum, Tronco, Tronco, Tronco, Nenhum, Tronco, Tronco, Tronco])
            ,(Relva, [Arvore, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Arvore])
            ]

estadoInicial :: Mapa -> Estado
estadoInicial mapaInicial = (MenuJogar, Jogo (Jogador (0,0)) mapaInicial, 0)

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

--substituir menus por imagens (tipo o tanks risingfan)
drawState :: Estado -> Picture
drawState (MenuJogar, jogo, _) = Pictures [Color blue $ drawOption "Jogar", Translate 0 (-70) $ drawOption "Creditos", Translate 0 (-140) $ drawOption "Sair"]
drawState (MenuCreditos, jogo, _) = Pictures [drawOption "Jogar", Color blue $ Translate 0 (-70) $ drawOption "Creditos", Translate 0 (-140) $ drawOption "Sair"]
drawState (MenuSair, jogo, _) = Pictures [drawOption "Jogar", Translate 0 (-70) $ drawOption "Creditos", Color blue $ Translate 0 (-140) $ drawOption "Sair"]

drawState (Creditos, jogo, _) = Pictures [ Color blue $ drawOption "Feito por:"]

drawState (Pausa VoltarJogo, jogo, _) = Pictures [Color blue $ drawOption "Voltar ao Jogo", Translate 0 (-70) $ drawOption "Guardar e Sair",Translate 0 (-140) $ drawOption "Sair"]
drawState (Pausa GuardarSair, jogo, _) = Pictures [drawOption "Voltar ao Jogo", Color blue $ Translate 0 (-70) $ drawOption "Guardar e Sair",Translate 0 (-140) $ drawOption "Sair"]
drawState (Pausa SairPausa, jogo, _) = Pictures [drawOption "Voltar ao Jogo", Translate 0 (-70) $ drawOption "Guardar e Sair", Color blue $ Translate 0 (-140) $ drawOption "Sair"]

drawState (Jogar, Jogo (Jogador (x,y)) (Mapa _ ll), score) = Pictures (drawLines (0,0) ll ++ [Translate i j jogador] ++ [Color yellow $ Translate (-290) 460 $ Scale 0.5 0.5 $ drawOption ("Score: " ++ show (truncate score))] )
  where i = fromIntegral (-320 + x*80)
        j = fromIntegral (400 - (y*100))

drawState (Perdeu JogarDeNovo,jogo,score) = Pictures [Color red $ drawOption ("Score: " ++ show (truncate score)), Translate 0 (-70) $ Color blue $ drawOption "Jogar de Novo",Translate 0 (-140) $ drawOption "Sair"]
drawState (Perdeu PerdeuSair,jogo,score) = Pictures [Color red $ drawOption ("Score: " ++ show (truncate score)), Translate 0 (-70) $ drawOption "Jogar de Novo",Translate 0 (-140) $ Color blue $ drawOption "Sair"]


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
event (EventKey (SpecialKey KeyUp) Down _ _) (MenuJogar, jogo, score) = (MenuSair, jogo, score)
event (EventKey (SpecialKey KeyDown) Down _ _) (MenuJogar, jogo, score) = (MenuCreditos, jogo, score)
event (EventKey (SpecialKey KeyEnter) Down _ _) (MenuJogar, jogo, score) = (Jogar, jogo, score)
event (EventKey (SpecialKey KeyUp) Down _ _) (MenuCreditos, jogo, score) = (MenuJogar, jogo, score)
event (EventKey (SpecialKey KeyDown) Down _ _) (MenuCreditos, jogo, score) = (MenuSair, jogo, score)
event (EventKey (SpecialKey KeyEnter) Down _ _) (MenuCreditos, jogo, score) = (Creditos, jogo, score)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Creditos, jogo, score) = (MenuCreditos, jogo, score)
event (EventKey (SpecialKey KeyUp) Down _ _) (MenuSair, jogo, score) = (MenuCreditos, jogo, score)
event (EventKey (SpecialKey KeyDown) Down _ _) (MenuSair, jogo, score) = (MenuJogar, jogo, score)
event (EventKey (SpecialKey KeyEnter) Down _ _) (MenuSair, jogo, _) = error "Fim de Jogo"

--movimentos no jogo
event (EventKey (Char 'q') Down _ _) (Jogar, jogo, score) = (Pausa VoltarJogo, jogo, score)
event (EventKey (SpecialKey key) Down _ _) (Jogar, Jogo (Jogador c) (Mapa l ll), score) = case key of
  KeyUp -> (Jogar, Jogo (Jogador (moveJogador c l ll (Move Cima))) (Mapa l ll), score)
  KeyDown -> (Jogar, Jogo (Jogador (moveJogador c l ll (Move Baixo))) (Mapa l ll), score)
  KeyRight -> (Jogar, Jogo (Jogador (moveJogador c l ll (Move Direita))) (Mapa l ll), score)
  KeyLeft -> (Jogar, Jogo (Jogador (moveJogador c l ll (Move Esquerda))) (Mapa l ll), score)
  _ -> (Jogar, Jogo (Jogador c) (Mapa l ll),score) 

--menu de pausa
event (EventKey (SpecialKey KeyUp) Down _ _) (Pausa VoltarJogo, jogo, score) = (Pausa SairPausa, jogo, score)
event (EventKey (SpecialKey KeyDown) Down _ _) (Pausa VoltarJogo, jogo, score) = (Pausa GuardarSair, jogo, score)
event (EventKey (SpecialKey KeyUp) Down _ _) (Pausa GuardarSair, jogo, score) = (Pausa VoltarJogo, jogo, score)
event (EventKey (SpecialKey KeyDown) Down _ _) (Pausa GuardarSair, jogo, score) = (Pausa SairPausa, jogo, score)
event (EventKey (SpecialKey KeyUp) Down _ _) (Pausa SairPausa, jogo, score) = (Pausa GuardarSair, jogo, score)
event (EventKey (SpecialKey KeyDown) Down _ _) (Pausa SairPausa, jogo, score) = (Pausa VoltarJogo, jogo, score)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Pausa VoltarJogo, jogo, score) = (Jogar, jogo, score)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Pausa SairPausa, jogo, score) = error "Fim de Jogo"
event (EventKey (Char 'q') Down _ _) (Pausa _, jogo, score) = (Jogar, jogo, score)

event (EventKey (SpecialKey KeyUp) Down _ _) (Perdeu JogarDeNovo, jogo, score) = (Perdeu PerdeuSair, jogo, score)
event (EventKey (SpecialKey KeyDown) Down _ _) (Perdeu JogarDeNovo, jogo, score) = (Perdeu PerdeuSair, jogo, score)
event (EventKey (SpecialKey KeyUp) Down _ _) (Perdeu PerdeuSair, jogo, score) = (Perdeu JogarDeNovo, jogo, score)
event (EventKey (SpecialKey KeyDown) Down _ _) (Perdeu PerdeuSair, jogo, score) = (Perdeu JogarDeNovo, jogo, score)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Perdeu JogarDeNovo, jogo, score) = (MenuJogar, jogo, 0)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Perdeu PerdeuSair, jogo, score) = error "Fim de Jogo"

event _ e = e
--time ->
--mover obstaculos com as velocidades (?)
--deslizar o mapa (?)
--score
time :: Int -> Float -> Estado -> Estado --a passagem do tempo é a movimentação dos obstáculos
--time n t (m, jogo@(Jogo (Jogador (x,y)) (Mapa l ll)), score) | m /= Jogar = (m,jogo, score) 
--                                                             | jogoTerminou jogo = (Perdeu JogarDeNovo, jogo, score)
--                                                             | otherwise = (m,deslizaJogo n (Jogo (Jogador (x,y)) (Mapa l (moveObstaculos ll (maxCasas (ll !! y) (x,y))))), score)
time n t e = e

main :: IO ()
main = do 
  n <- randomRIO (0,100)
  let randList = take 10 $ randoms (mkStdGen n)                 --
  let mapaInicial = gerarMapaInicial randList (Mapa 8 []) 10    --onde se define a largura e o número de linhas que o mapa tem
  play mainDisplay
       (greyN 0.25)      --background color
       1                --fps
       (estadoInicial mapaInicial)
       drawState
       event 
       (time n)

--TODO
--imagens para os menus
--imagens para obstáculos e jogador
--arranjar deslizaJogo -> mais suave e rapidez
--contar o score com o passar do tempo
--forma de com o jogar novamente o mapa ser diferente ou usar o mesmo mapa para as primeiras n linhas e o random só vem na nova geração de linhas comk a deslizaJogo
--,...
--salvar progresso no GuardarSair