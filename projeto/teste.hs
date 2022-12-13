module Teste where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import LI
import T3
import System.Random


data World = World Jogo

gerarMapaInicial :: [Int]  --random number
               -> Mapa --mapa gerado (no início vazio)
               -> Int  --número de iterações da função (número de linhas do mapa inicial)
               -> Mapa
gerarMapaInicial _ m 0 = m
gerarMapaInicial randList m n = gerarMapaInicial (init(randList)) (estendeMapa m (randList !! n)) (n-1)


mainDisplay :: Display
mainDisplay = InWindow "Tanks" (1280,640) (0,0)

initialWorld :: World
initialWorld = undefined

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
  let mapaInicial = gerarMapaInicial l1
  play mainDisplay
       (greyN 0.25)      --background color
       15                --fps
       initialWorld
       drawState
       event 
       time