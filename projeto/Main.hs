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

type Estado = (ModoJogo, Jogo, Score, [Picture])

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
            ,(Estrada 2, [Carro, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum])
            ,(Estrada 1, [Carro, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum])
            ,(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Nenhum, Nenhum, Nenhum, Nenhum])
            ,(Relva, [Arvore, Arvore, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Arvore])
            ,(Relva, [Arvore, Nenhum, Arvore, Nenhum, Nenhum, Nenhum, Arvore, Arvore])
            ,(Rio 2, [Tronco, Tronco, Nenhum, Tronco, Tronco, Nenhum, Nenhum, Nenhum])
            ,(Rio (-1), [Nenhum, Tronco, Tronco, Tronco, Nenhum, Tronco, Tronco, Tronco])
            ,(Relva, [Arvore, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Nenhum, Arvore])
            ]

baseMapa :: Mapa
baseMapa = Mapa 8 [(Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore]),(Relva,[Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Arvore])]

estadoInicial :: Mapa -> [Picture] -> Estado
estadoInicial mapaInicial images = (MenuNovoJogo, Jogo (Jogador (4,8)) mapaInicial, 0, images)

--estadoInicial mapaInicial = (MenuNovoJogo, Jogo (Jogador (2,0)) mapaInicial)

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

drawState :: Estado -> IO Picture
drawState (MenuNovoJogo, jogo, _, images) = return (head images)
drawState (MenuContinuarJogo, jogo ,_, images) = return (images !! 1)
drawState (MenuControlos, jogo, _, images) = return (images !! 2)
drawState (MenuCreditos, jogo, _, images) = return (images !! 3)
drawState (MenuSair, jogo, _, images) = return (images !! 4)

drawState (Controlos, jogo, _, images) = return (images !! 5)
drawState (Creditos, jogo, _, images) = return (Pictures [ Color blue $ drawOption "Feito por:"]) --images !! 6

--depois dos creditos +1 na images
drawState (Pausa VoltarJogo, jogo, _, images) = return (images !! 6)
drawState (Pausa GuardarSair, jogo, _, images) = return (images !! 7)
drawState (Pausa SairPausa, jogo, _, images) = return (images !! 8)

--jogador -> last images
drawState (Jogar, Jogo (Jogador (x,y)) (Mapa _ ll), score, images) = return (Pictures (drawLines (0,0) ll images ++ [Translate i j jogador] ++ [Color yellow $ Translate (-290) 460 $ Scale 0.5 0.5 $ drawOption ("Score: " ++ show (truncate score))]))
  where i = fromIntegral (-320 + x*80)
        j = fromIntegral (400 - (y*100))

--depois dos creditos +1 na images
drawState (Perdeu JogarDeNovo, jogo, score, images) = return (Pictures((images !! 9) : [Color (makeColorI 117 28 25 1) $ Translate (-10) 138 $ Scale 0.5 0.45 $ Text (show score)])) 
drawState (Perdeu PerdeuSair, jogo, score, images) = return (Pictures((images !! 10) : [Color (makeColorI 117 28 25 1) $ Translate (-10) 138 $ Scale 0.5 0.45 $ Text (show score)])) 

--desenhar cada linha e chamar a função para os obstáculos
--depois dos creditos +1 na images
drawLines :: (Int, Int) -> [(Terreno,[Obstaculo])] -> [Picture] -> [Picture]
drawLines _  [] _ = []
drawLines (x,y) ((t,os):ts) images = case t of 
  Relva -> translate (-280) (fromIntegral(450 - (y*100))) (Pictures (translate 280 0 (images !! 11) : drawObstacles x (t,os) images)) : drawLines (x,y+1) ts images
  Rio _ -> translate (-280) (fromIntegral(450 - (y*100))) (Pictures (translate 280 0 (images !! 12) : drawObstacles x (t,os) images)) : drawLines (x,y+1) ts images 
  Estrada _ -> translate (-280) (fromIntegral(450 - (y*100))) (Pictures (translate 280 0 (images !! 13) : drawObstacles x (t,os) images)) : drawLines (x,y+1) ts images

--desenhar os obstáculos (chamada a cada linha)
--depois dos creditos +1 na images
drawObstacles :: Int -> (Terreno,[Obstaculo]) -> [Picture] -> [Picture]
drawObstacles _ (_,[]) _ = []
drawObstacles x (Relva,o:os) images = case o of 
  Nenhum -> translate (fromIntegral(x*80)) 0 Blank : drawObstacles (x+1) (Relva,os) images
  Arvore -> translate (fromIntegral(x*80)) 0 (images !! 17) : drawObstacles (x+1) (Relva,os) images 
drawObstacles x (Rio v,o:os) images = case o of 
  Nenhum -> translate (fromIntegral(x*80)) 0 Blank : drawObstacles (x+1) (Rio v,os) images
  Tronco -> translate (fromIntegral(x*80)) 0 (images !! 16) : drawObstacles (x+1) (Rio v,os) images 
drawObstacles x (Estrada v,o:os) images = case o of 
  Nenhum -> translate (fromIntegral(x*80)) 0 Blank : drawObstacles (x+1) (Estrada v,os) images 
  Carro -> translate (fromIntegral(x*80)) 0 carPic : drawObstacles (x+1) (Estrada v,os) images
    where carPic = if v > 0 then images !! 14 else images !! 15

event :: Event -> Estado -> IO Estado
--menu inicial
event (EventKey (SpecialKey KeyUp) Down _ _) (MenuNovoJogo, jogo, score, images) = return (MenuSair, jogo, score, images)
event (EventKey (SpecialKey KeyDown) Down _ _) (MenuNovoJogo, jogo, score, images) = return (MenuContinuarJogo, jogo, score, images)
event (EventKey (SpecialKey KeyEnter) Down _ _) (MenuNovoJogo, jogo, score, images) = return (Jogar, jogo, score, images)
event (EventKey (SpecialKey KeyUp) Down _ _) (MenuContinuarJogo, jogo, score, images) = return (MenuNovoJogo, jogo, score, images)
event (EventKey (SpecialKey KeyDown) Down _ _) (MenuContinuarJogo, jogo, score, images) = return (MenuControlos, jogo, score, images)
event (EventKey (SpecialKey KeyEnter) Down _ _) (MenuContinuarJogo, jogo, score, images) = do 
  save <- readFile "save.txt"
  let sJogo = read $ head $ lines save
  let sScore = read $ last $ lines save
  if null save then return (Jogar, jogo, score, images) else return (Jogar, sJogo, sScore, images)
event (EventKey (SpecialKey KeyUp) Down _ _) (MenuControlos, jogo, score, images) = return (MenuContinuarJogo, jogo, score, images)
event (EventKey (SpecialKey KeyDown) Down _ _) (MenuControlos, jogo, score, images) = return (MenuCreditos, jogo, score, images)
event (EventKey (SpecialKey KeyEnter) Down _ _) (MenuControlos, jogo, score, images) = return (Controlos, jogo, score, images)
event (EventKey (SpecialKey KeyUp) Down _ _) (MenuCreditos, jogo, score, images) = return (MenuControlos, jogo, score, images)
event (EventKey (SpecialKey KeyDown) Down _ _) (MenuCreditos, jogo, score, images) = return (MenuSair, jogo, score, images)
event (EventKey (SpecialKey KeyEnter) Down _ _) (MenuCreditos, jogo, score, images) = return (Creditos, jogo, score, images)
event (EventKey (SpecialKey KeyUp) Down _ _) (MenuSair, jogo, score, images) = return (MenuCreditos, jogo, score, images)
event (EventKey (SpecialKey KeyDown) Down _ _) (MenuSair, jogo, score, images) = return (MenuNovoJogo, jogo, score, images)
event (EventKey (SpecialKey KeyEnter) Down _ _) (MenuSair, _ , _ , _) = error "Fim de Jogo"
event (EventKey (SpecialKey KeyEnter) Down _ _) (Controlos, jogo, score, images) = return (MenuControlos, jogo, score, images)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Creditos, jogo, score, images) = return (MenuCreditos, jogo, score, images)

--movimentos no jogo
event (EventKey (Char 'p') Down _ _) (Jogar, jogo, score, images) = return  (Pausa VoltarJogo, jogo, score, images)
event (EventKey (SpecialKey key) Down _ _) (Jogar, Jogo (Jogador c) (Mapa l ll), score, images) = case key of
  KeyUp -> return (Jogar, Jogo (Jogador (moveJogador c l ll (Move Cima))) (Mapa l ll), score, images)
  KeyDown -> return (Jogar, Jogo (Jogador (moveJogador c l ll (Move Baixo))) (Mapa l ll), score, images)
  KeyRight -> return (Jogar, Jogo (Jogador (moveJogador c l ll (Move Direita))) (Mapa l ll), score, images)
  KeyLeft -> return (Jogar, Jogo (Jogador (moveJogador c l ll (Move Esquerda))) (Mapa l ll), score, images)
  _ -> return (Jogar, Jogo (Jogador c) (Mapa l ll), score, images) 

--menu de pausa
event (EventKey (SpecialKey KeyUp) Down _ _) (Pausa VoltarJogo, jogo, score, images) = return (Pausa SairPausa, jogo, score, images)
event (EventKey (SpecialKey KeyDown) Down _ _) (Pausa VoltarJogo, jogo, score, images) = return (Pausa GuardarSair, jogo, score, images)
event (EventKey (SpecialKey KeyUp) Down _ _) (Pausa GuardarSair, jogo, score, images) = return (Pausa VoltarJogo, jogo, score, images)
event (EventKey (SpecialKey KeyDown) Down _ _) (Pausa GuardarSair, jogo, score, images) = return (Pausa SairPausa, jogo, score, images)
event (EventKey (SpecialKey KeyUp) Down _ _) (Pausa SairPausa, jogo, score, images) = return (Pausa GuardarSair, jogo, score, images)
event (EventKey (SpecialKey KeyDown) Down _ _) (Pausa SairPausa, jogo, score, images) = return (Pausa VoltarJogo, jogo, score, images)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Pausa VoltarJogo, jogo, score, images) = return (Jogar, jogo, score, images)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Pausa GuardarSair, jogo, score, _) = do
  writeFile "save.txt" (show jogo ++ "\n" ++ show score)
  error "Fim de Jogo"
event (EventKey (SpecialKey KeyEnter) Down _ _) (Pausa SairPausa, _ , _ , _) = do
  writeFile "save.txt" ""
  error "Fim de Jogo"
event (EventKey (Char 'p') Down _ _) (Pausa _, jogo, score, images) = return  (Jogar, jogo, score, images)

--menu perdeu
event (EventKey (SpecialKey KeyUp) Down _ _) (Perdeu JogarDeNovo, jogo, score, images) = return (Perdeu PerdeuSair, jogo, score, images)
event (EventKey (SpecialKey KeyDown) Down _ _) (Perdeu JogarDeNovo, jogo, score, images) = return (Perdeu PerdeuSair, jogo, score, images)
event (EventKey (SpecialKey KeyUp) Down _ _) (Perdeu PerdeuSair, jogo, score, images) = return (Perdeu JogarDeNovo, jogo, score, images)
event (EventKey (SpecialKey KeyDown) Down _ _) (Perdeu PerdeuSair, jogo, score, images) = return (Perdeu JogarDeNovo, jogo, score, images)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Perdeu JogarDeNovo, jogo, score, images) = do
  n <- randomRIO (0,100)
  let randList = take 7 $ randoms (mkStdGen n)
  let novoMapa = gerarMapa randList baseMapa 7
  return (MenuNovoJogo, Jogo (Jogador (4,8)) novoMapa, 0, images)
event (EventKey (SpecialKey KeyEnter) Down _ _) (Perdeu PerdeuSair, _ , _ , _ ) = do 
  writeFile "save.txt" ""
  error "Fim de Jogo"

event _ e = return e

time :: Float -> Estado -> IO Estado --a passagem do tempo é a movimentação dos obstáculos
time t (m, jogo@(Jogo (Jogador (x,y)) (Mapa l ll)), score, images) | m /= Jogar = return (m,jogo, score, images) 
                                                                   | jogoTerminou jogo = return (Perdeu JogarDeNovo, jogo, score, images)
                                                                   | otherwise = do 
                                                                          n <- randomRIO (0,100)
                                                                          return (m,deslizaJogo n (Jogo (Jogador (x,y)) (Mapa l (moveObstaculos ll (maxCasas (ll !! y) (x,y))))), score+0.25, images)
--time _ _ e = return e

main :: IO ()
main = do
  menuNovoJogo <- loadJuicyPNG "img/MenuNovoJogo.png"
  menuContinuarJogo <- loadJuicyPNG "img/MenuContinuarJogo.png"
  menuControlos <- loadJuicyPNG "img/MenuControlos.png"
  menuCreditos <- loadJuicyPNG "img/MenuCreditos.png"
  menuSair <- loadJuicyPNG "img/MenuSair.png"
  controlos <- loadJuicyPNG "img/Controlos.png"
  --creditos <- loadJuicyPNG FilePath
  pausaVoltarJogo <- loadJuicyPNG "img/pausaVoltarJogo.png"
  pausaGuardarSair <- loadJuicyPNG "img/pausaGuardarSair.png"
  pausaSair <- loadJuicyPNG "img/pausaSair.png"
  perdeuJogarDnv <- loadJuicyPNG "img/perdeuJogardeNovo.png"
  perdeuSair <- loadJuicyPNG "img/perdeuSair.png"
  relva <- loadJuicyPNG "img/relva.png"
  estrada <- loadJuicyPNG "img/estrada.png"
  rio <- loadJuicyPNG "img/rio.png"
  carro1d <- loadJuicyPNG "img/carro1d.png"
  carro1e <- loadJuicyPNG "img/carro1e.png"
  tronco <- loadJuicyPNG "img/tronco.png"
  arvore <- loadJuicyPNG "img/arvore.png"
  --jogador <- loadJuicyPNG FilePath
  let images = map fromJust [menuNovoJogo, menuContinuarJogo, menuControlos, menuCreditos, menuSair, controlos, pausaVoltarJogo, pausaGuardarSair, pausaSair, perdeuJogarDnv, perdeuSair, relva, estrada, rio, carro1d, carro1e, tronco, arvore]
  n <- randomRIO (0,100)
  let randList = take 7 $ randoms (mkStdGen n)                 --
  let mapaInicial = gerarMapa randList baseMapa 7    --onde se define a largura e o número de linhas que o mapa tem
  playIO mainDisplay
       (greyN 0.25)      --background color
       1                --fps
       (estadoInicial mapaInicial images)
       drawState
       event 
       time

--TODO
--imagens para jogador // acabar créditos
--arranjar os indices das imagens depois de por creditos
--arranjar deslizaJogo -> mais suave e rapidez [usar o score para regularizar o deslizaJogo(?) ou juntar um parâmetro do tempo ao estado]
--fazer o score direito
--carros diferentes para diferentes tamanhos (?)
--fazer uma documentação simples bonitinha (só a descrever as funções) // para a Tarefa 5 também
--testes Tarefa 5, uma coisinha simples