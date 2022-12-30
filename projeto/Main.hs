{- |
Module      : Main
Description : Aplicação gráfica do projeto
Copyright   : José Luís Matos Ribeiro  <a105108@alunos.uminho.pt>
              Gabriel Pereira Ribeiro  <a104171@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2022/23.
-}
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
import Graphics.Gloss.Juicy
import Data.Maybe

data ModoJogo = MenuNovoJogo      -- ^ Opção 'jogar' selecionada no menu
              | MenuContinuarJogo -- ^ Opção 'continuar a jogar'
              | MenuControlos     -- ^ Opção 'controlos'
              | MenuCreditos      -- ^ Opção 'créditos'
              | MenuSair          -- ^ Opção 'sair' selecionada no menu 
              | Jogar             -- ^ Enquanto joga
              | Controlos         -- ^ Janela dos controlos
              | Creditos          -- ^ Parte dos créditos
              | Perdeu MenuPerdeu -- ^ Menu quando o jogo termina
              | Pausa MenuPausa   -- ^ Menu de pausa
              deriving Eq

-- | Várias opções do menu de pausa.
data MenuPausa = VoltarJogo | GuardarSair | SairPausa 
  deriving Eq

-- | Várias opções do menu de fim de jogo.
data MenuPerdeu = JogarDeNovo | PerdeuSair
  deriving Eq

-- | Pontuação do jogador.
type Score = Float

-- | Contador auxiliar de tempo.
type Tempo = Float

-- | Estado do jogo para guardar informações: o modo de jogo, o jogo, a pontuação, a lista de imagens usadas, a direção do último movimento e o contador do tempo.
type Estado = (ModoJogo, Jogo, Score, [Picture], Direcao, Tempo)

-- | Função para gerar um mapa aleatório.
gerarMapa :: [Int] -> Mapa -> Int -> Mapa
gerarMapa _ m 0 = m
gerarMapa randList m n = gerarMapa (init randList) (estendeMapa m (randList !! (n-1))) (n-1)

-- | Janela do jogo.
mainDisplay :: Display
mainDisplay = InWindow "Crossy Road" (640,1000) (0,0)

-- | Base do mapa fixa, para melhorar a jogabilidade.
baseMapa :: Mapa
baseMapa = Mapa 8 [(Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore]),(Relva,[Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Arvore])]

-- | Estado inicial do jogo.
estadoInicial :: Mapa -> [Picture] -> Estado
estadoInicial mapaInicial images = (MenuNovoJogo, Jogo (Jogador (4,8)) mapaInicial, 0, images, Cima, 0)

-- | Função para desenhar o jogo.
drawState :: Estado -> IO Picture
drawState (MenuNovoJogo, jogo, _, images, _, _) = return (head images)
drawState (MenuContinuarJogo, jogo ,_, images, _, _) = return (images !! 1)
drawState (MenuControlos, jogo, _, images, _ , _) = return (images !! 2)
drawState (MenuCreditos, jogo, _, images, _, _) = return (images !! 3)
drawState (MenuSair, jogo, _, images, _, _) = return (images !! 4)
drawState (Controlos, jogo, _, images, _, _) = return (images !! 5)
drawState (Creditos, jogo, _, images, _, _) = return (images !! 6)
drawState (Pausa VoltarJogo, jogo, _, images, _, _) = return (images !! 7)
drawState (Pausa GuardarSair, jogo, _, images, _, _) = return (images !! 8)
drawState (Pausa SairPausa, jogo, _, images, _, _) = return (images !! 9)
drawState (Jogar, Jogo (Jogador (x,y)) (Mapa _ ll), score, images, lastMove, _) = return (Pictures (drawLines (0,0) ll images ++ [Translate i j playerPic] ++ [Translate (-259) 485 $ last images] ++ [Color (makeColorI 117 28 25 1) $ Translate (-198) 473 $ Scale 0.24 0.22 $ Text (show $ truncate score)]))
  where i = fromIntegral (-280 + x*80)
        j = fromIntegral (450 - (y*100))
        playerPic = case lastMove of
          Cima -> images !! 23
          Baixo -> images !! 24
          Direita -> images !! 25
          Esquerda -> images !! 26
drawState (Perdeu JogarDeNovo, jogo, score, images, _, _) = return (Pictures((images !! 10) : [Color (makeColorI 117 28 25 1) $ Translate (-10) 138 $ Scale 0.5 0.45 $ Text (show $ truncate score)])) 
drawState (Perdeu PerdeuSair, jogo, score, images, _, _) = return (Pictures((images !! 11) : [Color (makeColorI 117 28 25 1) $ Translate (-10) 138 $ Scale 0.5 0.45 $ Text (show $ truncate score)])) 

-- | Função para desenhar cada linha linha do mapa.
drawLines :: (Int, Int) -> [(Terreno,[Obstaculo])] -> [Picture] -> [Picture]
drawLines _  [] _ = []
drawLines (x,y) ((t,os):ts) images = case t of 
  Relva -> Translate 0 (fromIntegral(450 - (y*100))) (Pictures ((images !! 12) : drawObstacles x (t,os) images)) : drawLines (x,y+1) ts images
  Rio _ -> Translate 0 (fromIntegral(450 - (y*100))) (Pictures ((images !! 13) : drawObstacles x (t,os) images)) : drawLines (x,y+1) ts images 
  Estrada _ -> Translate 0 (fromIntegral(450 - (y*100))) (Pictures ((images !! 14) : drawObstacles x (t,os) images)) : drawLines (x,y+1) ts images

-- | Função para desenhar os obstáculos numa linha do mapa.
drawObstacles :: Int -> (Terreno,[Obstaculo]) -> [Picture] -> [Picture]
drawObstacles _ (_,[]) _ = []
drawObstacles x (Relva,o:os) images = case o of 
  Nenhum -> Translate (fromIntegral(x*80 - 280)) 0 Blank : drawObstacles (x+1) (Relva,os) images
  Arvore -> Translate (fromIntegral(x*80 - 280)) 0 (images !! 21) : drawObstacles (x+1) (Relva,os) images 
drawObstacles x (Rio v,o:os) images = case o of 
  Nenhum -> Translate (fromIntegral(x*80 - 280)) 0 Blank : drawObstacles (x+1) (Rio v,os) images
  Tronco -> Translate (fromIntegral(x*80 - 280)) 0 (images !! 22) : drawObstacles (x+1) (Rio v,os) images 
drawObstacles _ (Estrada v,os) images | head(head gos) == Carro && head(last gos) == Carro = if lenHead + lenLast == 2 then Translate (-320) 0 carPic2 : drawMiddle 1 v (tail $ init gos) images ++ [Translate 320 0 carPic2] 
                                                                                             else if lenHead == 2 then Translate (-280) 0 carPic3 : drawMiddle 2 v (tail $ init gos) images ++ [Translate 360 0 carPic3]
                                                                                                  else Translate (-360) 0 carPic3 : drawMiddle 1 v (tail $ init gos) images ++ [Translate 280 0 carPic3]
                                      | otherwise = drawMiddle 0 v gos images 
  where gos = group os
        lenHead = length (head gos)
        lenLast = length (last gos)
        carPic2 = if v > 0 then images !! 17 else images !! 18
        carPic3 = if v > 0 then images !! 19 else images !! 20

-- | Função para desenhar os obstáculos da Estrada excluíndo, se for o caso, os obstáculos das pontas.
drawMiddle :: Int -> Int ->  [[Obstaculo]] -> [Picture] -> [Picture]
drawMiddle _ _ [] _ = []
drawMiddle x v ([Nenhum]:os) images = Translate (fromIntegral(x*80)) 0 Blank : drawMiddle (x+1) v os images
drawMiddle x v ((Nenhum:ns):os) images = Translate (fromIntegral(x*80)) 0 Blank : drawMiddle (x+1) v (ns:os) images
drawMiddle x v ((Carro:ns):os) images | length ns + 1 == 1 = if v > 0 then Translate (fromIntegral(x*80 - 280)) 0 (images !! 15) : drawMiddle (x+1) v os images else Translate (fromIntegral(x*80 - 280)) 0 (images !! 16) : drawMiddle (x+1) v os images
                                      | length ns + 1 == 2 = if v > 0 then Translate (fromIntegral(x*80 - 240)) 0 (images !! 17) : drawMiddle (x+2) v os images else Translate (fromIntegral(x*80 - 240)) 0 (images !! 18) : drawMiddle (x+2) v os images
                                      | otherwise = if v > 0 then Translate (fromIntegral(x*80 - 200)) 0 (images !! 19) : drawMiddle (x+3) v os images else Translate (fromIntegral(x*80 - 200)) 0 (images !! 20) : drawMiddle (x+3) v os images

-- | Função para reagir às teclas pressionadas pelo utilizador
event :: Event -> Estado -> IO Estado
-- Eventos no menu inicial
event (EventKey (SpecialKey KeyUp) Down _ _) (MenuNovoJogo, jogo, score, images, lastMove, tempo) = return (MenuSair, jogo, score, images, lastMove, tempo)
event (EventKey (SpecialKey KeyDown) Down _ _) (MenuNovoJogo, jogo, score, images, lastMove, tempo) = return (MenuContinuarJogo, jogo, score, images, lastMove, tempo)
event (EventKey (SpecialKey KeyEnter) Down _ _) (MenuNovoJogo, jogo, score, images, lastMove, tempo) = return (Jogar, jogo, score, images, lastMove, tempo)
event (EventKey (SpecialKey KeyUp) Down _ _) (MenuContinuarJogo, jogo, score, images, lastMove, tempo) = return (MenuNovoJogo, jogo, score, images, lastMove, tempo)
event (EventKey (SpecialKey KeyDown) Down _ _) (MenuContinuarJogo, jogo, score, images, lastMove, tempo) = return (MenuControlos, jogo, score, images, lastMove, tempo)
event (EventKey (SpecialKey KeyEnter) Down _ _) (MenuContinuarJogo, jogo, score, images, _, _) = do 
  save <- readFile "save.txt"
  let sJogo = read $ head $ lines save
  let sScore = read $ lines save !! 1
  let sMove = read $ last $ lines save
  if null save then return (Jogar, jogo, score, images, Cima, 0) else return (Jogar, sJogo, sScore, images, sMove, 0)
event (EventKey (SpecialKey KeyUp) Down _ _) (MenuControlos, jogo, score, images, lastMove, tempo) = return (MenuContinuarJogo, jogo, score, images, lastMove, tempo)
event (EventKey (SpecialKey KeyDown) Down _ _) (MenuControlos, jogo, score, images, lastMove, tempo) = return (MenuCreditos, jogo, score, images, lastMove, tempo)
event (EventKey (SpecialKey KeyEnter) Down _ _) (MenuControlos, jogo, score, images, lastMove, tempo) = return (Controlos, jogo, score, images, lastMove, tempo)
event (EventKey (SpecialKey KeyUp) Down _ _) (MenuCreditos, jogo, score, images, lastMove, tempo) = return (MenuControlos, jogo, score, images, lastMove, tempo)
event (EventKey (SpecialKey KeyDown) Down _ _) (MenuCreditos, jogo, score, images, lastMove, tempo) = return (MenuSair, jogo, score, images, lastMove, tempo)
event (EventKey (SpecialKey KeyEnter) Down _ _) (MenuCreditos, jogo, score, images, lastMove, tempo) = return (Creditos, jogo, score, images, lastMove, tempo)
event (EventKey (SpecialKey KeyUp) Down _ _) (MenuSair, jogo, score, images, lastMove, tempo) = return (MenuCreditos, jogo, score, images, lastMove, tempo)
event (EventKey (SpecialKey KeyDown) Down _ _) (MenuSair, jogo, score, images, lastMove, tempo) = return (MenuNovoJogo, jogo, score, images, lastMove, tempo)
event (EventKey (SpecialKey KeyEnter) Down _ _) (MenuSair, _ , _ , _, _, _) = error "Fim de Jogo"
event (EventKey (SpecialKey KeyEnter) Down _ _) (Controlos, jogo, score, images, lastMove, tempo) = return (MenuControlos, jogo, score, images, lastMove, tempo)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Creditos, jogo, score, images, lastMove, tempo) = return (MenuCreditos, jogo, score, images, lastMove, tempo)

-- Eventos no menu de pausa
event (EventKey (Char 'p') Down _ _) (Jogar, jogo, score, images, lastMove, tempo) = return  (Pausa VoltarJogo, jogo, score, images, lastMove, tempo)
event (EventKey (SpecialKey key) Down _ _) (Jogar, Jogo (Jogador c) (Mapa l ll), score, images, lastMove, tempo) = case key of
  KeyUp -> return (Jogar, Jogo (Jogador (moveJogador c l ll (Move Cima))) (Mapa l ll), score, images, Cima, tempo)
  KeyDown -> return (Jogar, Jogo (Jogador (moveJogador c l ll (Move Baixo))) (Mapa l ll), score, images, Baixo, tempo)
  KeyRight -> return (Jogar, Jogo (Jogador (moveJogador c l ll (Move Direita))) (Mapa l ll), score, images, Direita, tempo)
  KeyLeft -> return (Jogar, Jogo (Jogador (moveJogador c l ll (Move Esquerda))) (Mapa l ll), score, images, Esquerda, tempo)
  _ -> return (Jogar, Jogo (Jogador c) (Mapa l ll), score, images, lastMove, tempo) 

-- Eventos no menu de pausa
event (EventKey (SpecialKey KeyUp) Down _ _) (Pausa VoltarJogo, jogo, score, images, lastMove, tempo) = return (Pausa SairPausa, jogo, score, images, lastMove, tempo)
event (EventKey (SpecialKey KeyDown) Down _ _) (Pausa VoltarJogo, jogo, score, images, lastMove, tempo) = return (Pausa GuardarSair, jogo, score, images, lastMove, tempo)
event (EventKey (SpecialKey KeyUp) Down _ _) (Pausa GuardarSair, jogo, score, images, lastMove, tempo) = return (Pausa VoltarJogo, jogo, score, images, lastMove, tempo)
event (EventKey (SpecialKey KeyDown) Down _ _) (Pausa GuardarSair, jogo, score, images, lastMove, tempo) = return (Pausa SairPausa, jogo, score, images, lastMove, tempo)
event (EventKey (SpecialKey KeyUp) Down _ _) (Pausa SairPausa, jogo, score, images, lastMove, tempo) = return (Pausa GuardarSair, jogo, score, images, lastMove, tempo)
event (EventKey (SpecialKey KeyDown) Down _ _) (Pausa SairPausa, jogo, score, images, lastMove, tempo) = return (Pausa VoltarJogo, jogo, score, images, lastMove, tempo)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Pausa VoltarJogo, jogo, score, images, lastMove, _) = return (Jogar, jogo, score, images, lastMove, 0)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Pausa GuardarSair, jogo, score, _, lastMove, _) = do
  writeFile "save.txt" (show jogo ++ "\n" ++ show score ++ "\n" ++ show lastMove)
  error "Fim de Jogo"
event (EventKey (SpecialKey KeyEnter) Down _ _) (Pausa SairPausa, _ , _ , _, _, _) = do
  writeFile "save.txt" ""
  error "Fim de Jogo"
event (EventKey (Char 'p') Down _ _) (Pausa _, jogo, score, images, lastMove, _) = return (Jogar, jogo, score, images, lastMove, 0)

-- Eventos no menu de fim de jogo
event (EventKey (SpecialKey KeyUp) Down _ _) (Perdeu JogarDeNovo, jogo, score, images, lastMove, tempo) = return (Perdeu PerdeuSair, jogo, score, images, lastMove, tempo)
event (EventKey (SpecialKey KeyDown) Down _ _) (Perdeu JogarDeNovo, jogo, score, images, lastMove, tempo) = return (Perdeu PerdeuSair, jogo, score, images, lastMove, tempo)
event (EventKey (SpecialKey KeyUp) Down _ _) (Perdeu PerdeuSair, jogo, score, images, lastMove, tempo) = return (Perdeu JogarDeNovo, jogo, score, images, lastMove, tempo)
event (EventKey (SpecialKey KeyDown) Down _ _) (Perdeu PerdeuSair, jogo, score, images, lastMove, tempo) = return (Perdeu JogarDeNovo, jogo, score, images, lastMove, tempo)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Perdeu JogarDeNovo, jogo, score, images, _, _) = do
  n <- randomRIO (0,100)
  let randList = take 7 $ randoms (mkStdGen n)
  let novoMapa = gerarMapa randList baseMapa 7
  return (MenuNovoJogo, Jogo (Jogador (4,8)) novoMapa, 0, images, Cima, 0)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Perdeu PerdeuSair, _ , _ , _ , _, _) = do 
  writeFile "save.txt" ""
  error "Fim de Jogo"
event _ e = return e

-- | Função que define a passagem do tempo no jogo e que executa algo à medida que o tempo passa.
time :: Float -> Estado -> IO Estado
time _ (m, jogo@(Jogo (Jogador (x,y)) (Mapa l ll)), score, images, lastMove, tempo) | m /= Jogar = return (m,jogo, score, images, lastMove, tempo) 
                                                                                    | jogoTerminou jogo = return (Perdeu JogarDeNovo, jogo, score, images, lastMove, tempo)
                                                                                    | otherwise = case tempo of 
                                                                                        1 -> do 
                                                                                            n <- randomRIO (0,100)
                                                                                            return (m,deslizaJogo n (Jogo (Jogador (x,y)) (Mapa l (moveObstaculos ll (maxCasas (ll !! y) (x,y))))), score+0.25, images, lastMove,0)              
                                                                                        _ -> return (m, Jogo (Jogador (x,y)) (Mapa l (moveObstaculos ll (maxCasas (ll !! y) (x,y)))), score+0.25, images, lastMove,tempo+0.25)

-- | Função para correr o jogo.
main :: IO ()
main = do
  menuNovoJogo <- loadJuicyPNG "img/MenuNovoJogo.png"
  menuContinuarJogo <- loadJuicyPNG "img/MenuContinuarJogo.png"
  menuControlos <- loadJuicyPNG "img/MenuControlos.png"
  menuCreditos <- loadJuicyPNG "img/MenuCreditos.png"
  menuSair <- loadJuicyPNG "img/MenuSair.png"
  controlos <- loadJuicyPNG "img/Controlos.png"
  creditos <- loadJuicyPNG "img/Creditos.png"
  pausaVoltarJogo <- loadJuicyPNG "img/pausaVoltarJogo.png"
  pausaGuardarSair <- loadJuicyPNG "img/pausaGuardarSair.png"
  pausaSair <- loadJuicyPNG "img/pausaSair.png"
  perdeuJogarDnv <- loadJuicyPNG "img/perdeuJogardeNovo.png"
  perdeuSair <- loadJuicyPNG "img/perdeuSair.png"
  relva <- loadJuicyPNG "img/relva.png"
  rio <- loadJuicyPNG "img/rio.png"
  estrada <- loadJuicyPNG "img/estrada.png"
  carro1d <- loadJuicyPNG "img/carro1d.png"
  carro1e <- loadJuicyPNG "img/carro1e.png"
  carro2d <- loadJuicyPNG "img/carro2d.png"
  carro2e <- loadJuicyPNG "img/carro2e.png"
  carro3d <- loadJuicyPNG "img/carro3d.png"
  carro3e <- loadJuicyPNG "img/carro3e.png"
  arvore <- loadJuicyPNG "img/arvore.png"
  tronco <- loadJuicyPNG "img/tronco.png"
  jogadorCima <- loadJuicyPNG "img/galinhaFrente.png"
  jogadorBaixo <- loadJuicyPNG "img/galinhaTras.png"
  jogadorEsquerda <- loadJuicyPNG "img/galinhaEsquerda.png"
  jogadorDireita <- loadJuicyPNG "img/galinhaDireita.png"
  scoreTxt <- loadJuicyPNG "img/scoreTxt.png"
  let images = map fromJust [menuNovoJogo, menuContinuarJogo, menuControlos, menuCreditos, menuSair, controlos, creditos, pausaVoltarJogo, pausaGuardarSair, pausaSair, 
                             perdeuJogarDnv, perdeuSair, relva, rio, estrada, carro1d, carro1e, carro2d, carro2e, carro3d, carro3e, arvore, tronco, jogadorCima, jogadorBaixo, jogadorDireita, jogadorEsquerda, scoreTxt]
  n <- randomRIO (0,100)
  let randList = take 7 $ randoms (mkStdGen n)
  let mapaInicial = gerarMapa randList baseMapa 7
  playIO mainDisplay
       (greyN 0.25)
       2
       (estadoInicial mapaInicial images)
       drawState
       event 
       time