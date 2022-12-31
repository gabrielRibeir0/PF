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
type Score = Int

-- | Contador auxiliar de tempo.
type Tempo = Int

-- | Estado do jogo para guardar informações: o modo de jogo, o jogo, a pontuação e a melhor pontuação do jogo, a lista de imagens usadas, a direção do último movimento, o contador do tempo e a melhor pontuação que o jogador já teve.
type Estado = (ModoJogo, Jogo, Score, Score, [Picture], Direcao, Tempo, Score)

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
estadoInicial mapaInicial images = (MenuNovoJogo, Jogo (Jogador (4,9)) mapaInicial, 0, 0, images, Cima, 0, 0)

-- | Função para desenhar o jogo.
drawState :: Estado -> IO Picture
drawState (MenuNovoJogo, jogo, _, _, images, _, _, _) = return (head images)
drawState (MenuContinuarJogo, jogo ,_, _, images, _, _, _) = return (images !! 1)
drawState (MenuControlos, jogo, _, _, images, _ , _, _) = return (images !! 2)
drawState (MenuCreditos, jogo, _, _, images, _, _, _) = return (images !! 3)
drawState (MenuSair, jogo, _, _, images, _, _, _) = return (images !! 4)
drawState (Controlos, jogo, _, _, images, _, _, _) = return (images !! 5)
drawState (Creditos, jogo, _, _, images, _, _, _) = return (images !! 6)
drawState (Pausa VoltarJogo, jogo, _, _, images, _, _, _) = return (images !! 7)
drawState (Pausa GuardarSair, jogo, _, _,images, _, _, _) = return (images !! 8)
drawState (Pausa SairPausa, jogo, _, _, images, _, _, _) = return (images !! 9)
drawState (Jogar, Jogo (Jogador (x,y)) (Mapa _ ll), score, topScore, images, lastMove, _, _) = return $ Pictures (drawLines (0,0) ll images ++ [Translate i j playerPic] ++ [Translate (-259) 485 $ last images] ++ [Color (makeColorI 117 28 25 1) $ Translate (-198) 473 $ Scale 0.24 0.22 $ Text (show score)])
  where i = fromIntegral (-280 + x*80)
        j = fromIntegral (450 - (y*100))
        playerPic = case lastMove of
          Cima -> images !! 24
          Baixo -> images !! 25
          Direita -> images !! 26
          Esquerda -> images !! 27
drawState (Perdeu JogarDeNovo, jogo, score, topScore, images, _, _, bestScore) = return (if topScore > bestScore then Pictures((images !! 10) : Color yellow (Translate 8 93 $ Scale 0.39 0.3 $ Text (show topScore)): [Color (makeColorI 117 28 25 1) $ Translate (-10) 138 $ Scale 0.5 0.3 $ Text (show topScore)]) else Pictures((images !! 10) : Color (makeColorI 117 28 25 1) (Translate 8 93 $ Scale 0.4 0.37 $ Text (show bestScore)): [Color (makeColorI 117 28 25 1) $ Translate (-10) 138 $ Scale 0.5 0.45 $ Text (show topScore)]))
drawState (Perdeu PerdeuSair, jogo, score, topScore, images, _, _, bestScore) = return (if topScore > bestScore then Pictures((images !! 11) : Color yellow (Translate 8 93 $ Scale 0.39 0.3 $ Text (show topScore)): [Color (makeColorI 117 28 25 1) $ Translate (-10) 138 $ Scale 0.5 0.3 $ Text (show topScore)]) else Pictures((images !! 11) : Color (makeColorI 117 28 25 1) (Translate 8 93 $ Scale 0.4 0.37 $ Text (show bestScore)): [Color (makeColorI 117 28 25 1) $ Translate (-10) 138 $ Scale 0.5 0.45 $ Text (show topScore)]))


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
  Tronco -> if v > 0 then Translate (fromIntegral(x*80 - 280)) 0 (images !! 22) : drawObstacles (x+1) (Rio v,os) images else Translate (fromIntegral(x*80 - 280)) 0 (images !! 23) : drawObstacles (x+1) (Rio v,os) images 
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

-- | Função para reagir às teclas pressionadas pelo utilizador.
event :: Event -> Estado -> IO Estado
-- Eventos no menu inicial
event (EventKey (SpecialKey KeyUp) Down _ _) (MenuNovoJogo, jogo, score, topScore, images, lastMove, tempo, bestScore) = return (MenuSair, jogo, score, topScore, images, lastMove, tempo, bestScore)
event (EventKey (SpecialKey KeyDown) Down _ _) (MenuNovoJogo, jogo, score, topScore, images, lastMove, tempo, bestScore) = return (MenuContinuarJogo, jogo, score, topScore, images, lastMove, tempo, bestScore)
event (EventKey (SpecialKey KeyEnter) Down _ _) (MenuNovoJogo, jogo, score, topScore, images, lastMove, tempo, bestScore) = do
  pontuacao <- readFile "bestScore.txt"
  let savedBestScore = read pontuacao
  return (Jogar, jogo, score, topScore, images, lastMove, tempo, savedBestScore)
event (EventKey (SpecialKey KeyUp) Down _ _) (MenuContinuarJogo, jogo, score, topScore, images, lastMove, tempo, bestScore) = return (MenuNovoJogo, jogo, score, topScore, images, lastMove, tempo, bestScore)
event (EventKey (SpecialKey KeyDown) Down _ _) (MenuContinuarJogo, jogo, score, topScore, images, lastMove, tempo, bestScore) = return (MenuControlos, jogo, score, topScore, images, lastMove, tempo, bestScore)
event (EventKey (SpecialKey KeyEnter) Down _ _) (MenuContinuarJogo, jogo, score, topScore, images, _, _, bestScore) = do
  pontuacao <- readFile "bestScore.txt"
  save <- readFile "save.txt"
  let savedJogo = read $ head $ lines save 
  let savedScore = read $ lines save !! 1
  let savedTopScore = read $ lines save !! 2
  let savedMove = read $ last $ lines save
  let savedBestScore = read pontuacao
  if null save then return (Jogar, jogo, score, topScore, images, Cima, 0, savedBestScore) else return (Jogar, savedJogo, savedScore, savedTopScore, images, savedMove, 0, savedBestScore)
event (EventKey (SpecialKey KeyUp) Down _ _) (MenuControlos, jogo, score, topScore, images, lastMove, tempo, bestScore) = return (MenuContinuarJogo, jogo, score, topScore, images, lastMove, tempo, bestScore)
event (EventKey (SpecialKey KeyDown) Down _ _) (MenuControlos, jogo, score, topScore, images, lastMove, tempo, bestScore) = return (MenuCreditos, jogo, score, topScore, images, lastMove, tempo, bestScore)
event (EventKey (SpecialKey KeyEnter) Down _ _) (MenuControlos, jogo, score, topScore, images, lastMove, tempo, bestScore) = return (Controlos, jogo, score, topScore, images, lastMove, tempo, bestScore)
event (EventKey (SpecialKey KeyUp) Down _ _) (MenuCreditos, jogo, score, topScore, images, lastMove, tempo, bestScore) = return (MenuControlos, jogo, score, topScore, images, lastMove, tempo, bestScore)
event (EventKey (SpecialKey KeyDown) Down _ _) (MenuCreditos, jogo, score, topScore, images, lastMove, tempo, bestScore) = return (MenuSair, jogo, score, topScore, images, lastMove, tempo, bestScore)
event (EventKey (SpecialKey KeyEnter) Down _ _) (MenuCreditos, jogo, score, topScore, images, lastMove, tempo, bestScore) = return (Creditos, jogo, score, topScore, images, lastMove, tempo, bestScore)
event (EventKey (SpecialKey KeyUp) Down _ _) (MenuSair, jogo, score, topScore, images, lastMove, tempo, bestScore) = return (MenuCreditos, jogo, score, topScore, images, lastMove, tempo, bestScore)
event (EventKey (SpecialKey KeyDown) Down _ _) (MenuSair, jogo, score, topScore, images, lastMove, tempo, bestScore) = return (MenuNovoJogo, jogo, score, topScore, images, lastMove, tempo, bestScore)
event (EventKey (SpecialKey KeyEnter) Down _ _) (MenuSair, _ , _ , _, _, _, _, _) = error "Fim de Jogo"
event (EventKey (SpecialKey KeyEnter) Down _ _) (Controlos, jogo, score, topScore, images, lastMove, tempo, bestScore) = return (MenuControlos, jogo, score, topScore, images, lastMove, tempo, bestScore)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Creditos, jogo, score, topScore, images, lastMove, tempo, bestScore) = return (MenuCreditos, jogo, score, topScore, images, lastMove, tempo, bestScore)

-- Eventos durante o jogo
event (EventKey (Char 'p') Down _ _) (Jogar, jogo, score, topScore, images, lastMove, tempo, bestScore) = return (Pausa VoltarJogo, jogo, score, topScore, images, lastMove, tempo, bestScore)
event (EventKey (SpecialKey key) Down _ _) (Jogar, Jogo (Jogador (x,y)) (Mapa l ll), score, topScore, images, lastMove, tempo, bestScore) = case key of
  KeyUp -> if yUp /= y then return (Jogar, Jogo (Jogador (xUp, yUp)) (Mapa l ll), score + 1, max (score + 1) topScore, images, Cima, tempo, bestScore) else return (Jogar, Jogo (Jogador (xUp, yUp)) (Mapa l ll), score, topScore, images, Cima, tempo, bestScore)
  KeyDown -> if yDown /= y then return (Jogar, Jogo (Jogador (xDown, yDown)) (Mapa l ll), score - 1, topScore, images, Baixo, tempo, bestScore) else return (Jogar, Jogo (Jogador (xDown, yDown)) (Mapa l ll), score, topScore, images, Baixo, tempo, bestScore)
  KeyRight -> return (Jogar, Jogo (Jogador (moveJogador (x,y) l ll (Move Direita))) (Mapa l ll), score, topScore, images, Direita, tempo, bestScore)
  KeyLeft -> return (Jogar, Jogo (Jogador (moveJogador (x,y) l ll (Move Esquerda))) (Mapa l ll), score, topScore, images, Esquerda, tempo, bestScore)
  _ -> return (Jogar, Jogo (Jogador (x,y)) (Mapa l ll), score, topScore, images, lastMove, tempo, bestScore)
  where (xUp, yUp) = moveJogador (x,y) l ll (Move Cima)
        (xDown, yDown) = moveJogador (x,y) l ll (Move Baixo)

-- Eventos no menu de pausa
event (EventKey (SpecialKey KeyUp) Down _ _) (Pausa VoltarJogo, jogo, score, topScore, images, lastMove, tempo, bestScore) = return (Pausa SairPausa, jogo, score, topScore, images, lastMove, tempo, bestScore)
event (EventKey (SpecialKey KeyDown) Down _ _) (Pausa VoltarJogo, jogo, score, topScore, images, lastMove, tempo, bestScore) = return (Pausa GuardarSair, jogo, score, topScore, images, lastMove, tempo, bestScore)
event (EventKey (SpecialKey KeyUp) Down _ _) (Pausa GuardarSair, jogo, score, topScore, images, lastMove, tempo, bestScore) = return (Pausa VoltarJogo, jogo, score, topScore, images, lastMove, tempo, bestScore)
event (EventKey (SpecialKey KeyDown) Down _ _) (Pausa GuardarSair, jogo, score, topScore, images, lastMove, tempo, bestScore) = return (Pausa SairPausa, jogo, score, topScore, images, lastMove, tempo, bestScore)
event (EventKey (SpecialKey KeyUp) Down _ _) (Pausa SairPausa, jogo, score, topScore, images, lastMove, tempo, bestScore) = return (Pausa GuardarSair, jogo, score, topScore, images, lastMove, tempo, bestScore)
event (EventKey (SpecialKey KeyDown) Down _ _) (Pausa SairPausa, jogo, score, topScore, images, lastMove, tempo, bestScore) = return (Pausa VoltarJogo, jogo, score, topScore, images, lastMove, tempo, bestScore)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Pausa VoltarJogo, jogo, score, topScore, images, lastMove, _, bestScore) = return (Jogar, jogo, score, topScore, images, lastMove, 0, bestScore)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Pausa GuardarSair, jogo, score, topScore, _, lastMove, _, bestScore) = do
  writeFile "save.txt" (show jogo ++ "\n" ++ show score ++ "\n" ++ show topScore ++ "\n"++ show lastMove)
  writeFile "bestScore.txt" (show (max bestScore topScore))
  error "Fim de Jogo"
event (EventKey (SpecialKey KeyEnter) Down _ _) (Pausa SairPausa, _ , topScore , _, _, _, _, bestScore) = do
  writeFile "save.txt" ""
  writeFile "bestScore.txt" (show bestScore)
  error "Fim de Jogo"
event (EventKey (Char 'p') Down _ _) (Pausa _, jogo, score, topScore, images, lastMove, _, bestScore) = return (Jogar, jogo, score, topScore, images, lastMove, 0, bestScore)

-- Eventos no menu de fim de jogo
event (EventKey (SpecialKey KeyUp) Down _ _) (Perdeu JogarDeNovo, jogo, score, topScore, images, lastMove, tempo, bestScore) = return (Perdeu PerdeuSair, jogo, score, topScore, images, lastMove, tempo, bestScore)
event (EventKey (SpecialKey KeyDown) Down _ _) (Perdeu JogarDeNovo, jogo, score, topScore, images, lastMove, tempo, bestScore) = return (Perdeu PerdeuSair, jogo, score, topScore, images, lastMove, tempo, bestScore)
event (EventKey (SpecialKey KeyUp) Down _ _) (Perdeu PerdeuSair, jogo, score, topScore, images, lastMove, tempo, bestScore) = return (Perdeu JogarDeNovo, jogo, score, topScore, images, lastMove, tempo, bestScore)
event (EventKey (SpecialKey KeyDown) Down _ _) (Perdeu PerdeuSair, jogo, score, topScore, images, lastMove, tempo, bestScore) = return (Perdeu JogarDeNovo, jogo, score, topScore, images, lastMove, tempo, bestScore)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Perdeu JogarDeNovo, jogo, score, topScore, images, _, _, bestScore) = do
  writeFile "bestScore.txt" (show (max bestScore topScore))
  n <- randomRIO (0,100)
  let randList = take 7 $ randoms (mkStdGen n)
  let novoMapa = gerarMapa randList baseMapa 7
  return (MenuNovoJogo, Jogo (Jogador (4,8)) novoMapa, 0, 0, images, Cima, 0, bestScore)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Perdeu PerdeuSair,_ , _ , topScore, _, _, _, bestScore) = do 
  writeFile "save.txt" (show (max bestScore topScore))
  writeFile "bestScore.txt" (show (max bestScore topScore))
  error "Fim de Jogo"
event _ e = return e

-- | Função que define a passagem do tempo no jogo e que executa algo à medida que o tempo passa.
time :: Float -> Estado -> IO Estado
time _ (m, jogo@(Jogo (Jogador (x,y)) (Mapa l ll)), score, topScore, images, lastMove, tempo, bestScore) | m /= Jogar = return (m,jogo, score, topScore, images, lastMove, tempo, bestScore) 
                                                                                                         | jogoTerminou jogo = return (Perdeu JogarDeNovo, jogo, score, topScore, images, lastMove, tempo, bestScore)
                                                                                                         | otherwise = case tempo of
                                                                                                              32 -> return (m,jogo, score, topScore, images, lastMove,0, bestScore) 
                                                                                                              30 -> do 
                                                                                                                    n <- randomRIO (0,100)
                                                                                                                    return (m,deslizaJogo n (Jogo (Jogador (x,y) ) (Mapa l ll)), score, topScore, images, lastMove,tempo+1, bestScore)
                                                                                                              _ | mod tempo 10 == 0-> do
                                                                                                                    return (m, Jogo (Jogador (moveJogador (x,y) l ll Parado)) (Mapa l (moveObstaculos ll (maxCasas (ll !! y) (x,y)))), score, topScore, images, lastMove,tempo+1, bestScore)
                                                                                                              _ -> return (m,jogo, score, topScore, images, lastMove,tempo+1, bestScore)

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
  troncoDireita <- loadJuicyPNG "img/troncoDireita.png"
  troncoEsquerda <- loadJuicyPNG "img/troncoEsquerda.png"
  jogadorCima <- loadJuicyPNG "img/galinhaFrente.png"
  jogadorBaixo <- loadJuicyPNG "img/galinhaTras.png"
  jogadorEsquerda <- loadJuicyPNG "img/galinhaEsquerda.png"
  jogadorDireita <- loadJuicyPNG "img/galinhaDireita.png"
  scoreTxt <- loadJuicyPNG "img/scoreTxt.png"
  let images = map fromJust [menuNovoJogo, menuContinuarJogo, menuControlos, menuCreditos, menuSair, controlos, creditos, pausaVoltarJogo, pausaGuardarSair, pausaSair, 
                             perdeuJogarDnv, perdeuSair, relva, rio, estrada, carro1d, carro1e, carro2d, carro2e, carro3d, carro3e, arvore, troncoDireita, troncoEsquerda, jogadorCima, jogadorBaixo, jogadorDireita, jogadorEsquerda, scoreTxt]
  n <- randomRIO (0,100)
  let randList = take 7 $ randoms (mkStdGen n)
  let mapaInicial = gerarMapa randList baseMapa 7
  playIO mainDisplay
       (greyN 0.25)
       20
       (estadoInicial mapaInicial images)
       drawState
       event 
       time