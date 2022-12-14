module T2 where

import LI
import T1 (obsV2, validaPassagemEntreArvores)

estendeMapa :: Mapa -> Int -> Mapa
estendeMapa (Mapa l ll) n
    | obsV2 (velocidadeAleatoria n terreno t) (listaObstaculo n l terreno) && validaPassagemEntreArvores (velocidadeAleatoria n terreno t, listaObstaculo n l terreno) ll = Mapa l ((velocidadeAleatoria n terreno t, listaObstaculo n l terreno): ll)
    | otherwise = estendeMapa (Mapa l ll) (n+1)
    where terreno | null ll = Relva
                  | otherwise =  escolheTerreno n (proximosTerrenosValidos (Mapa l ll))
          t | null ll = Relva
            | otherwise = fst(head ll)

proximosTerrenosValidos :: Mapa -> [Terreno]
proximosTerrenosValidos (Mapa _ ((Rio _, _):(Rio _, _):(Rio _, _):(Rio _, _):t)) = [Estrada 0, Relva]
proximosTerrenosValidos (Mapa _ ((Relva,_):(Relva,_):(Relva,_):(Relva,_):(Relva,_):t)) = [Rio 0, Estrada 0]
proximosTerrenosValidos (Mapa _ ((Estrada _, _):(Estrada _, _):(Estrada _, _):(Estrada _, _):(Estrada _,_):t)) = [Relva, Rio 0]
proximosTerrenosValidos (Mapa _ _) = [Relva, Rio 0, Estrada 0]

escolheTerreno :: Int -> [Terreno] -> Terreno
escolheTerreno n (h:t) | mod n 3 == 0 = h
                       | mod n 3 == 1 = head t
                       | otherwise = last t

velocidadeAleatoria :: Int -> Terreno -> Terreno -> Terreno
velocidadeAleatoria _ Relva _ = Relva
velocidadeAleatoria n (Rio v) (Rio v0) | v0 > 0 = if even n then Rio (-1) else Rio (-2)
                                       | otherwise =  if even n then Rio 1 else Rio 2
velocidadeAleatoria n (Rio v) _ | mod n 4 == 0 = Rio 1
                                | mod n 4 == 1 = Rio (-1)
                                | mod n 4 == 2 = Rio 2
                                | otherwise = Rio (-2)
velocidadeAleatoria n (Estrada v) _ | mod n 4 == 0 = Estrada 1
                                    | mod n 4 == 1 = Estrada (-1)
                                    | mod n 4 == 2 = Estrada 2
                                    | otherwise = Estrada (-2)

proximosObstaculosValidos :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosObstaculosValidos l (Rio _, []) = [Nenhum,Tronco]
proximosObstaculosValidos l (Estrada _, []) = [Nenhum,Carro]
proximosObstaculosValidos l (Relva , os) | length os >= l = [] 
                                         | otherwise = if elem Nenhum os then [Nenhum, Arvore] else [Nenhum]
proximosObstaculosValidos l (Rio _, os) | length os >= l = []
                                        | length os > 4 = if elem Nenhum (take 5 os) then [Nenhum,Tronco] else [Nenhum]
                                        | otherwise = if elem Nenhum os && not(elem Tronco os) then [Tronco] else if elem Nenhum os then [Nenhum,Tronco] else [Nenhum] 
proximosObstaculosValidos l (Estrada  _, os) | length os >= l = []
                                             | length os > 2 = if elem Nenhum (take 3 os) then [Nenhum,Carro] else [Nenhum]
                                             | otherwise = if elem Nenhum os then [Nenhum, Carro] else [Nenhum]

escolheObstaculo :: Int -> Largura -> (Terreno, [Obstaculo]) -> Obstaculo
escolheObstaculo n l (t,os) | even n = head obsv
                            | otherwise = last obsv
    where obsv = proximosObstaculosValidos l (t,os)

listaObstaculo :: Int -> Largura -> Terreno -> [Obstaculo]
listaObstaculo n 0 t = []
listaObstaculo n l t = escolheObstaculo n l (t,o) : o
    where o = listaObstaculo (n + (mod n 5)) (l-1) t