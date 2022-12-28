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
import System.Random ( mkStdGen, randomRIO, Random(randoms) )
import Graphics.Gloss.Juicy
import Data.Maybe

data ModoJogo = MenuNovoJogo      --opção 'jogar' selecionada no menu
              | MenuContinuarJogo --opção 'continuar a jogar'
              | MenuControlos     --opção 'controlos'
              | MenuCreditos      --opção 'créditos'
              | MenuSair          --opção 'sair' selecionada no menu 
              | Jogar             --enquanto joga
              | Controlos         --janela dos controlos
              | Creditos          --parte dos créditos
              | Perdeu MenuPerdeu --menu quando o jogo termina
              | Pausa MenuPausa   --menu de pausa
              deriving Eq

data MenuPausa = VoltarJogo | GuardarSair | SairPausa 
  deriving Eq

data MenuPerdeu = JogarDeNovo | PerdeuSair
  deriving Eq

type Score = Float

type Estado = (ModoJogo, Jogo, Score, [Picture], Direcao)

gerarMapa :: [Int]  --lista de números aleatórios
                 -> Mapa   --mapa gerado (no início vazio)
                 -> Int    --número de iterações da função (número de linhas do mapa inicial)
                 -> Mapa
gerarMapa _ m 0 = m
gerarMapa randList m n = gerarMapa (init randList) (estendeMapa m (randList !! (n-1))) (n-1)

mainDisplay :: Display
mainDisplay = InWindow "Crossy Road" (640,1000) (0,0)

--mapa de exemplo
exmap :: Mapa
exmap = Mapa 8 [(Relva, [Nenhum, Arvore, Nenhum, Nenhum, Arvore, Nenhum, Nenhum, Arvore])
            ,(Relva, [Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum])
            ,(Estrada 2, [Carro, Nenhum, Nenhum, Carro, Carro, Carro, Nenhum, Carro])
            ,(Estrada 1, [Carro, Nenhum, Carro, Nenhum, Nenhum, Nenhum, Carro, Carro])
            ,(Estrada (1), [Carro, Carro, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Carro])
            ,(Relva, [Arvore, Arvore, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Arvore])
            ,(Relva, [Arvore, Nenhum, Arvore, Nenhum, Nenhum, Nenhum, Arvore, Arvore])
            ,(Rio 2, [Tronco, Tronco, Nenhum, Tronco, Tronco, Nenhum, Nenhum, Nenhum])
            ,(Rio (-1), [Nenhum, Tronco, Tronco, Tronco, Nenhum, Tronco, Tronco, Tronco])
            ,(Relva, [Arvore, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Arvore])
            ]

baseMapa :: Mapa
baseMapa = Mapa 8 [(Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore]),(Relva,[Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Arvore])]

estadoInicial :: Mapa -> [Picture] -> Estado
--estadoInicial mapaInicial images = (MenuNovoJogo, Jogo (Jogador (4,8)) mapaInicial, 0, images, Cima)

estadoInicial mapaInicial images = (MenuNovoJogo, Jogo (Jogador (4,8)) exmap, 0, images, Cima)

drawOption :: String -> Picture
drawOption option = Translate (-50) 0 $ Scale (0.5) (0.5) $ Text option

drawState :: Estado -> IO Picture
drawState (MenuNovoJogo, jogo, _, images, _) = return (head images)
drawState (MenuContinuarJogo, jogo ,_, images, _) = return (images !! 1)
drawState (MenuControlos, jogo, _, images, _) = return (images !! 2)
drawState (MenuCreditos, jogo, _, images, _) = return (images !! 3)
drawState (MenuSair, jogo, _, images, _) = return (images !! 4)

drawState (Controlos, jogo, _, images, _) = return (images !! 5)
drawState (Creditos, jogo, _, images, _) = return (images !! 6)

drawState (Pausa VoltarJogo, jogo, _, images, _) = return (images !! 7)
drawState (Pausa GuardarSair, jogo, _, images, _) = return (images !! 8)
drawState (Pausa SairPausa, jogo, _, images, _) = return (images !! 9)

--score a aparecer no mapa (?)
drawState (Jogar, Jogo (Jogador (x,y)) (Mapa _ ll), score, images, lastMove) = return (Pictures (drawLines (0,0) ll images ++ [Translate i j playerPic] ++ [Color yellow $ Translate (-290) 460 $ Scale 0.5 0.5 $ drawOption ("Score: " ++ show (truncate score))]))
  where i = fromIntegral (-280 + x*80)
        j = fromIntegral (450 - (y*100))
        playerPic = case lastMove of
          Cima -> images !! 23
          Baixo -> images !! 24
          Direita -> images !! 25
          Esquerda -> last images

drawState (Perdeu JogarDeNovo, jogo, score, images, _) = return (Pictures((images !! 10) : [Color (makeColorI 117 28 25 1) $ Translate (-10) 138 $ Scale 0.5 0.45 $ Text (show score)])) 
drawState (Perdeu PerdeuSair, jogo, score, images, _) = return (Pictures((images !! 11) : [Color (makeColorI 117 28 25 1) $ Translate (-10) 138 $ Scale 0.5 0.45 $ Text (show score)])) 

--desenhar cada linha e chamar a função para os obstáculos
drawLines :: (Int, Int) -> [(Terreno,[Obstaculo])] -> [Picture] -> [Picture]
drawLines _  [] _ = []
drawLines (x,y) ((t,os):ts) images = case t of 
  Relva -> Translate 0 (fromIntegral(450 - (y*100))) (Pictures ((images !! 12) : drawObstacles x (t,os) images)) : drawLines (x,y+1) ts images
  Rio _ -> Translate 0 (fromIntegral(450 - (y*100))) (Pictures ((images !! 13) : drawObstacles x (t,os) images)) : drawLines (x,y+1) ts images 
  Estrada _ -> Translate 0 (fromIntegral(450 - (y*100))) (Pictures ((images !! 14) : drawObstacles x (t,os) images)) : drawLines (x,y+1) ts images

--desenhar os obstáculos (chamada a cada linha)
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

drawMiddle :: Int -> Int ->  [[Obstaculo]] -> [Picture] -> [Picture]
drawMiddle _ _ [] _ = []
drawMiddle x v ([Nenhum]:os) images = Translate (fromIntegral(x*80)) 0 Blank : drawMiddle (x+1) v os images
drawMiddle x v ((Nenhum:ns):os) images = Translate (fromIntegral(x*80)) 0 Blank : drawMiddle (x+1) v (ns:os) images
drawMiddle x v ((Carro:ns):os) images | length ns + 1 == 1 = if v > 0 then Translate (fromIntegral(x*80 - 280)) 0 (images !! 15) : drawMiddle (x+1) v os images else Translate (fromIntegral(x*80 - 280)) 0 (images !! 16) : drawMiddle (x+1) v os images
                                      | length ns + 1 == 2 = if v > 0 then Translate (fromIntegral(x*80 - 240)) 0 (images !! 17) : drawMiddle (x+2) v os images else Translate (fromIntegral(x*80 - 240)) 0 (images !! 18) : drawMiddle (x+2) v os images
                                      | otherwise = if v > 0 then Translate (fromIntegral(x*80 - 200)) 0 (images !! 19) : drawMiddle (x+3) v os images else Translate (fromIntegral(x*80 - 200)) 0 (images !! 20) : drawMiddle (x+3) v os images

event :: Event -> Estado -> IO Estado
--menu inicial
event (EventKey (SpecialKey KeyUp) Down _ _) (MenuNovoJogo, jogo, score, images, lastMove) = return (MenuSair, jogo, score, images, lastMove)
event (EventKey (SpecialKey KeyDown) Down _ _) (MenuNovoJogo, jogo, score, images, lastMove) = return (MenuContinuarJogo, jogo, score, images, lastMove)
event (EventKey (SpecialKey KeyEnter) Down _ _) (MenuNovoJogo, jogo, score, images, lastMove) = return (Jogar, jogo, score, images, lastMove)
event (EventKey (SpecialKey KeyUp) Down _ _) (MenuContinuarJogo, jogo, score, images, lastMove) = return (MenuNovoJogo, jogo, score, images, lastMove)
event (EventKey (SpecialKey KeyDown) Down _ _) (MenuContinuarJogo, jogo, score, images, lastMove) = return (MenuControlos, jogo, score, images, lastMove)
event (EventKey (SpecialKey KeyEnter) Down _ _) (MenuContinuarJogo, jogo, score, images, _) = do 
  save <- readFile "save.txt"
  let sJogo = read $ head $ lines save
  let sScore = read $ lines save !! 2
  let sMove = read $ last $ lines save
  if null save then return (Jogar, jogo, score, images, Cima) else return (Jogar, sJogo, sScore, images, sMove)
event (EventKey (SpecialKey KeyUp) Down _ _) (MenuControlos, jogo, score, images, lastMove) = return (MenuContinuarJogo, jogo, score, images, lastMove)
event (EventKey (SpecialKey KeyDown) Down _ _) (MenuControlos, jogo, score, images, lastMove) = return (MenuCreditos, jogo, score, images, lastMove)
event (EventKey (SpecialKey KeyEnter) Down _ _) (MenuControlos, jogo, score, images, lastMove) = return (Controlos, jogo, score, images, lastMove)
event (EventKey (SpecialKey KeyUp) Down _ _) (MenuCreditos, jogo, score, images, lastMove) = return (MenuControlos, jogo, score, images, lastMove)
event (EventKey (SpecialKey KeyDown) Down _ _) (MenuCreditos, jogo, score, images, lastMove) = return (MenuSair, jogo, score, images, lastMove)
event (EventKey (SpecialKey KeyEnter) Down _ _) (MenuCreditos, jogo, score, images, lastMove) = return (Creditos, jogo, score, images, lastMove)
event (EventKey (SpecialKey KeyUp) Down _ _) (MenuSair, jogo, score, images, lastMove) = return (MenuCreditos, jogo, score, images, lastMove)
event (EventKey (SpecialKey KeyDown) Down _ _) (MenuSair, jogo, score, images, lastMove) = return (MenuNovoJogo, jogo, score, images, lastMove)
event (EventKey (SpecialKey KeyEnter) Down _ _) (MenuSair, _ , _ , _, _) = error "Fim de Jogo"
event (EventKey (SpecialKey KeyEnter) Down _ _) (Controlos, jogo, score, images, lastMove) = return (MenuControlos, jogo, score, images, lastMove)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Creditos, jogo, score, images, lastMove) = return (MenuCreditos, jogo, score, images, lastMove)

--movimentos no jogo
event (EventKey (Char 'p') Down _ _) (Jogar, jogo, score, images, lastMove) = return  (Pausa VoltarJogo, jogo, score, images, lastMove)
event (EventKey (SpecialKey key) Down _ _) (Jogar, Jogo (Jogador c) (Mapa l ll), score, images, lastMove) = case key of
  KeyUp -> return (Jogar, Jogo (Jogador (moveJogador c l ll (Move Cima))) (Mapa l ll), score, images, Cima)
  KeyDown -> return (Jogar, Jogo (Jogador (moveJogador c l ll (Move Baixo))) (Mapa l ll), score, images, Baixo)
  KeyRight -> return (Jogar, Jogo (Jogador (moveJogador c l ll (Move Direita))) (Mapa l ll), score, images, Direita)
  KeyLeft -> return (Jogar, Jogo (Jogador (moveJogador c l ll (Move Esquerda))) (Mapa l ll), score, images, Esquerda)
  _ -> return (Jogar, Jogo (Jogador c) (Mapa l ll), score, images, lastMove) 

--menu de pausa
event (EventKey (SpecialKey KeyUp) Down _ _) (Pausa VoltarJogo, jogo, score, images, lastMove) = return (Pausa SairPausa, jogo, score, images, lastMove)
event (EventKey (SpecialKey KeyDown) Down _ _) (Pausa VoltarJogo, jogo, score, images, lastMove) = return (Pausa GuardarSair, jogo, score, images, lastMove)
event (EventKey (SpecialKey KeyUp) Down _ _) (Pausa GuardarSair, jogo, score, images, lastMove) = return (Pausa VoltarJogo, jogo, score, images, lastMove)
event (EventKey (SpecialKey KeyDown) Down _ _) (Pausa GuardarSair, jogo, score, images, lastMove) = return (Pausa SairPausa, jogo, score, images, lastMove)
event (EventKey (SpecialKey KeyUp) Down _ _) (Pausa SairPausa, jogo, score, images, lastMove) = return (Pausa GuardarSair, jogo, score, images, lastMove)
event (EventKey (SpecialKey KeyDown) Down _ _) (Pausa SairPausa, jogo, score, images, lastMove) = return (Pausa VoltarJogo, jogo, score, images, lastMove)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Pausa VoltarJogo, jogo, score, images, lastMove) = return (Jogar, jogo, score, images, lastMove)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Pausa GuardarSair, jogo, score, _, lastMove) = do
  writeFile "save.txt" (show jogo ++ "\n" ++ show score ++ "\n" ++ show lastMove)
  error "Fim de Jogo"
event (EventKey (SpecialKey KeyEnter) Down _ _) (Pausa SairPausa, _ , _ , _, _) = do
  writeFile "save.txt" ""
  error "Fim de Jogo"
event (EventKey (Char 'p') Down _ _) (Pausa _, jogo, score, images, lastMove) = return (Jogar, jogo, score, images, lastMove)

--menu perdeu
event (EventKey (SpecialKey KeyUp) Down _ _) (Perdeu JogarDeNovo, jogo, score, images, lastMove) = return (Perdeu PerdeuSair, jogo, score, images, lastMove)
event (EventKey (SpecialKey KeyDown) Down _ _) (Perdeu JogarDeNovo, jogo, score, images, lastMove) = return (Perdeu PerdeuSair, jogo, score, images, lastMove)
event (EventKey (SpecialKey KeyUp) Down _ _) (Perdeu PerdeuSair, jogo, score, images, lastMove) = return (Perdeu JogarDeNovo, jogo, score, images, lastMove)
event (EventKey (SpecialKey KeyDown) Down _ _) (Perdeu PerdeuSair, jogo, score, images, lastMove) = return (Perdeu JogarDeNovo, jogo, score, images, lastMove)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Perdeu JogarDeNovo, jogo, score, images, _) = do
  n <- randomRIO (0,100)
  let randList = take 7 $ randoms (mkStdGen n)
  let novoMapa = gerarMapa randList baseMapa 7
  return (MenuNovoJogo, Jogo (Jogador (4,8)) novoMapa, 0, images, Cima)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Perdeu PerdeuSair, _ , _ , _ , _) = do 
  writeFile "save.txt" ""
  error "Fim de Jogo"

event _ e = return e

time :: Float -> Estado -> IO Estado --a passagem do tempo é a movimentação dos obstáculos
time t (m, jogo@(Jogo (Jogador (x,y)) (Mapa l ll)), score, images, lastMove) | m /= Jogar = return (m,jogo, score, images, lastMove) 
                                                                             | jogoTerminou jogo = return (Perdeu JogarDeNovo, jogo, score, images, lastMove)
                                                                             | otherwise = do 
                                                                                n <- randomRIO (0,100)
                                                                                return (m,deslizaJogo n (Jogo (Jogador (x,y)) (Mapa l (moveObstaculos ll (maxCasas (ll !! y) (x,y))))), score+0.25, images, lastMove)
--time _ e = return e

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
  let images = map fromJust [menuNovoJogo, menuContinuarJogo, menuControlos, menuCreditos, menuSair, controlos, creditos, pausaVoltarJogo, pausaGuardarSair, pausaSair, 
                             perdeuJogarDnv, perdeuSair, relva, rio, estrada, carro1d, carro1e, carro2d, carro2e, carro3d, carro3e, arvore, tronco, jogadorCima, jogadorBaixo, jogadorDireita, jogadorEsquerda]
  n <- randomRIO (0,100)
  let randList = take 7 $ randoms (mkStdGen n)
  let mapaInicial = gerarMapa randList baseMapa 7
  playIO mainDisplay
       (greyN 0.25)
       1
       (estadoInicial mapaInicial images)
       drawState
       event 
       time

--TODO
--acabar créditos
--arranjar deslizaJogo -> mais suave e rapidez [usar o score para regularizar o deslizaJogo(?) ou juntar um parâmetro do tempo ao estado]
--fazer o score direito > como funciona e ao aparecer no jogo
--fazer uma documentação simples bonitinha (só a descrever as funções) // para a Tarefa 5 também
--testes Tarefa 5, uma coisinha simples
--parâmetro de dificuldade (?)