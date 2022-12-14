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
                | Pausa     --menu de pausa     --quando o jogador decide sair do jogo

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

drawState :: Estado -> Picture
drawState (MenuJogar, jogo) = Pictures [Color blue $ drawOption "Jogar", Translate 0 (-70) $ drawOption "Sair"]
drawState (MenuSair, jogo) = Pictures [drawOption "Jogar", Color blue $ Translate 0 (-70) $ drawOption "Sair"]

event :: Event -> Estado -> Estado
event (EventKey (SpecialKey KeyUp) Down _ _) (MenuJogar, jogo) = (MenuSair, jogo)
event (EventKey (SpecialKey KeyDown) Down _ _) (MenuJogar, jogo) = (MenuSair, jogo)
event (EventKey (SpecialKey KeyEnter) Down _ _) (MenuJogar, jogo) = (Jogar, jogo)
event (EventKey (SpecialKey KeyUp) Down _ _) (MenuSair, jogo) = (MenuJogar, jogo)
event (EventKey (SpecialKey KeyDown) Down _ _) (MenuSair, jogo) = (MenuJogar, jogo)
event (EventKey (SpecialKey KeyEnter) Down _ _) (MenuSair, jogo) = error "Fim de Jogo"

time :: Float -> Estado -> Estado
time = undefined

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

{-play

:: forall world. Display    
Display mode.

-> Color    
Background color.

-> Int    
Number of simulation steps to take for each second of real time.

-> world    
The initial world.

-> (world -> Picture)    
An action to convert the world a picture.

-> (Event -> world -> world)    
A function to handle input events.

-> (Float -> world -> world)    
A function to step the world one iteration. It is passed the period of time (in seconds) needing to be advanced.

-> IO ()-}
