module Teste where

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

mainDisplay :: Display
mainDisplay = InWindow "Crossy Road" (640,1000) (0,0)

main :: IO ()
main = do
  menuNovoJogo <- loadJuicyPNG "img/MenuNovoJogo.png"
  menuContinuarJogo <- loadJuicyPNG "img/MenuContinuarJogo.png"
  menuControlos <- loadJuicyPNG "img/MenuControlos.png"
  menuCreditos <- loadJuicyPNG "img/MenuCreditos.png"
  menuSair <- loadJuicyPNG "img/MenuSair.png"
  controlos <- loadJuicyPNG "img/Controlos.png"
  relva <- loadJuicyPNG "img/relva.png"
  estrada <- loadJuicyPNG "img/estrada.png"
  let images = map fromJust [menuNovoJogo, menuContinuarJogo, menuControlos, menuCreditos, menuSair, controlos, relva, estrada]  --onde se define a largura e o número de linhas que o mapa tem
  display mainDisplay (greyN 0.5) (translate (0) (fromIntegral(450)) (Pictures ((images !! 6) : [circle 10])))
