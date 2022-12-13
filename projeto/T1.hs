module T1 where
import LI
import Data.List

mapaValido :: Mapa -> Bool
mapaValido (Mapa l []) = True
mapaValido (Mapa l ((Estrada v,(h:t)):y))
 | lIgualv l (h:t) == True && obstaculoValido l (Estrada v) (h:t) == True && obsV2 (Estrada v) (h:t) == True && terrenoSeguidos (tRS ((Estrada v,(h:t)):y)) == True = mapaValido (Mapa l y)
 | otherwise = False
mapaValido (Mapa l ((Rio v,(h:t)):y))
 | lIgualv l (h:t) == True && obstaculoValido l (Rio v) (h:t) == True && obsV2 (Rio v) (h:t) == True && terrenoSeguidos (tRS ((Rio v,(h:t)):y)) == True && riosS (Rio v) y == True = mapaValido (Mapa l y)
 | otherwise = False
mapaValido (Mapa l ((Relva,(h:t)):y))
 | lIgualv l (h:t) == True && obstaculoValido l (Relva) (h:t) == True && obsV2 (Relva) (h:t) == True && terrenoSeguidos (tRS ((Relva,(h:t)):y)) == True && validaPassagemEntreArvores (Relva,(h:t)) y == True = mapaValido (Mapa l y)
 | otherwise = False

terrenoValido :: Terreno -> Bool
terrenoValido (Rio v) 
 |v > 0 || v < 0 = True
 |otherwise = False
terrenoValido (Estrada v)
 |v > 0 || v < 0 = True
 |otherwise = False
terrenoValido (Relva) = True

obstaculoValido :: Largura -> Terreno -> [Obstaculo] -> Bool
obstaculoValido l (Relva) []
 |l>0 = True
 |otherwise = False
obstaculoValido l (Relva) (h:t)
 | terrenoValido Relva == True && h == Arvore = obstaculoValido (l-1) (Relva) t
 | terrenoValido Relva == True && h == Nenhum = obstaculoValido l (Relva) t
 | otherwise = False
obstaculoValido l (Estrada v) []
 |l>0 = True
 |otherwise = False
obstaculoValido l (Estrada v) (h:t)
 | terrenoValido (Estrada v) == True && h == Carro = obstaculoValido (l-1) (Estrada v) t
 | terrenoValido (Estrada v) == True && h == Nenhum = obstaculoValido l (Estrada v) t
 | otherwise = False
obstaculoValido l (Rio v) []
 |l>0 = True
 |otherwise = False
obstaculoValido l (Rio v) (h:t)
 | terrenoValido (Rio v) == True && h == Nenhum = obstaculoValido (l-1) (Rio v) t
 | terrenoValido (Rio v) == True && h == Tronco = obstaculoValido l (Rio v) t
 | otherwise = False

lIgualv :: Largura -> [Obstaculo] -> Bool
lIgualv x [] = False
lIgualv x (h:t)
 |length (h:t) == x = True
 |otherwise = False

obsV2 ::Terreno -> [Obstaculo] -> Bool
obsV2 (Estrada v) [] = False
obsV2 (Estrada v) (h:t)
 | any (>3) (ols (Estrada v) (group(h:t))) == True = False
 | otherwise = True
obsV2 (Rio v) [] = False
obsV2 (Rio v) (h:t)
 | any (>5) (ols (Rio v) (group(h:t))) == True = False
 | otherwise = True
obsV2 (Relva) (h:t) = True
obsV2 (Relva) [] = False

ols :: Terreno -> [[Obstaculo]] -> [Int]
ols (Estrada v) ((Carro: t):y)
 |length ((Carro:t):y) == 1 = length (Carro:t) : ols (Estrada v) y
 |Carro == head (last ((Carro:t):y)) = length (Carro:t) + length(last((Carro:t):y)) : ols (Estrada v) (init y)
 |otherwise = length (Carro:t) : ols (Estrada v) y
ols (Estrada v) ((Nenhum:t):y)
 |head(last((Nenhum:t):y)) == Carro = length(last((Nenhum:t):y)) : ols (Estrada v) (init y)
 |otherwise = ols (Estrada v) y
ols (Estrada v) [] = []
ols (Rio v) ((Tronco: t):y)
 |length ((Tronco:t):y) == 1 = length (Tronco:t) : ols (Rio v) y
 |Tronco == head (last ((Tronco:t):y)) = length (Tronco:t) + length(last((Tronco:t):y)) : ols (Rio v) (init y)
 |otherwise = length(Tronco:t) : ols (Rio v) y
ols (Rio v) ((Nenhum: t):y)
 |head(last((Nenhum:t):y)) == Tronco = length(last((Nenhum:t):y)) : ols (Rio v) (init y)
 |otherwise = ols (Rio v) y
ols (Rio v) [] = []

terrenoSeguidos:: [Terreno] -> Bool
terrenoSeguidos (h:t) = tAux2( group(tAux (h:t) ) )

tRS :: [(Terreno, [Obstaculo])] -> [Terreno]
tRS [] = []
tRS (h:t) = fst h : tRS t

tAux :: [Terreno] -> [Terreno]
tAux [] = []
tAux (Estrada v : t) = Estrada 0 : tAux t
tAux (Rio v : t) = Rio 0 : tAux t
tAux (Relva : t) = Relva : tAux t

tAux2 :: [[Terreno]] -> Bool
tAux2 [] = True
tAux2 ((Rio v:t):ts)
 | length (Rio v:t) > 4 = False
 | otherwise = tAux2 ts
tAux2 ((Estrada v :t):ts)
 | length (Estrada v :t) > 5 = False
 | otherwise = tAux2 ts
tAux2 ((Relva:t):ts)
 | length (Relva:t) > 5 = False
 | otherwise = tAux2 ts

riosS :: Terreno -> [(Terreno,[Obstaculo])] -> Bool
riosS _ [] = True
riosS (Rio v1) ((Rio v2, _):ts) = v1 * v2 < 0
riosS _ _ = True

validaPassagemEntreArvores:: (Terreno,[Obstaculo]) -> [(Terreno,[Obstaculo])] -> Bool
validaPassagemEntreArvores _ [] = True
validaPassagemEntreArvores (_,xs) ((Relva ,ys):t) = (Nenhum, Nenhum) `elem` zip xs ys
validaPassagemEntreArvores _ _ = True