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

data World = World Jogo

gerarMapaInicial :: [Int]  --lista de números aleatórios
                 -> Mapa   --mapa gerado (no início vazio)
                 -> Int    --número de iterações da função (número de linhas do mapa inicial)
                 -> Mapa
gerarMapaInicial _ m 0 = m
gerarMapaInicial randList m n = gerarMapaInicial (init randList) (estendeMapa m (randList !! (n-1))) (n-1)

mainDisplay :: Display
mainDisplay = InWindow "Crossy Road" (1280,640) (0,0)

initialWorld :: Mapa -> World
initialWorld m = World (Jogo (Jogador (0,0)) m)

drawState :: World -> Picture
drawState = undefined

event :: Event -> World -> World
event = undefined

time :: Float -> World -> World
time = undefined

main :: IO ()
main = do 
  n <- randomRIO (0,100)
  let l1 = take 5 $ randoms (mkStdGen n)
  let mapaInicial = gerarMapaInicial l1 (Mapa 4 []) 5    --onde se define a largura e o número de linhas que o mapa tem
  play mainDisplay
       (greyN 0.25)      --background color
       15                --fps
       initialWorld
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
