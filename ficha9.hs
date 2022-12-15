module Ficha9 where
import System.Random (randomRIO)

--ex
dataNasc :: IO (Int,Int,Int) 
dataNasc = do dia <- randomRIO (1,31)
              mes <- randomRIO (1,12)
              ano <- randomRIO (2000,2022)
              return (dia,mes,ano)

--1)a)função que sorteia os números para o jogo do bingo. Sempre que uma tecla é pressionada é dado um número aleatório entre 1 e 90. Não pode dar números repetidos e o programa termina depois de gerados os 90 números.
bingo :: IO ()
bingo = do nums <- sorteio []
           print (reverse nums)

sorteio :: [Int] -> IO [Int]
sorteio l | length l == 90 = return l
          | otherwise = do n <- randomRIO (1,90)
                           getChar
                           print n
                           if elem n l then sorteio l else sorteio (n:l)
