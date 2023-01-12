module Ficha9 where
import System.Random
import System.Random.Stateful
import Data.List
--ex
dataNasc :: IO (Int,Int,Int) 
dataNasc = do dia <- randomRIO (1,31)
              mes <- randomRIO (1,12)
              ano <- randomRIO (2000,2022)
              return (dia,mes,ano)

--1)a)função que sorteia os números para o jogo do bingo. Sempre que uma tecla é pressionada é dado um número aleatório entre 1 e 90. 
    --Não pode dar números repetidos e o programa termina depois de gerados os 90 números.
bingo :: IO ()
bingo = do nums <- sorteio []
           print (reverse nums)

sorteio :: [Int] -> IO [Int]
sorteio l | length l == 90 = return l
          | otherwise = do n <- randomRIO (1,90)
                           getChar
                           print n
                           if elem n l then sorteio l else sorteio (n:l)

{-1)b)função que implementa uma variante do jogo de descodificação de padrões Mastermind. O programa começa por gerar uma sequência secreta de 4 dígitos aleatórios que o jogador vai tentar descodificar.
Sempre que o jogadorintroduz uma sequência de 4 dígitos, o programa responde com o número de dígitos com o valor correto na posição correta e o número de dígitos com o valor correto na posição errada.
O jogo termina quando o jogador acertar a sequência secreta
-}
mastermind :: IO ()
mastermind = sequence (replicate 4 $ uniformRM (0,9) globalStdGen)
    >>= playMastermind

playMastermind :: [Int] -> IO ()
playMastermind numbers = do
    guess <- map read . words <$> getLine :: IO [Int]
    if length guess /= 4 then
        putStrLn "Sequencia invalida!"
        >> playMastermind numbers
    else
        let (right_loc, wrong_loc) = 
                foldr (\i (right_loc, wrong_loc) -> 
                    if guess !! i == numbers !! i then
                        (right_loc + 1, wrong_loc)
                    else if guess !! i `elem` numbers then
                        (right_loc, wrong_loc + 1)
                    else
                        (right_loc, wrong_loc)
                ) (0,0) [0..3] in
        if right_loc == 4 then
            putStrLn "Parabens! Acertaste na sequencia!"
        else
            putStrLn (unlines [
                "Valores corretos: " ++ show right_loc,
                "Valores no local errado: " ++ show wrong_loc
            ])
            >> playMastermind numbers

--2)tipo de dados para representar uma aposta do euromilhões, de 5 números entre 1 e 50 e 2 estrelas ente 1 e 9
data Aposta = Ap [Int] (Int,Int)

--a)função que testa se uma dada aposta é válida (tem 5 números e 2 estrelas, dentro dos valores aceites e não tem repetições)
valida :: Aposta -> Bool
valida (Ap nums@[n1,n2,n3,n4,n5] (e1,e2)) = 
    all (`elem` [1..50]) nums
    && nub nums == nums
    && e1 /= e2
    && e1 `elem` [1..12] && e2 `elem` [1..12]    
valida _ = False

--b)função que dada uma aposta e uma chave, calcula quantos números e quantas estrelas existem em comum nas duas apostas
comuns :: Aposta -> Aposta -> (Int, Int)
comuns (Ap nums (e1,e2)) (Ap nums_chave (e1_c, e2_c)) = 
    (length (nums `intersect` nums_chave),
     length (filter (`elem` [e1_c, e2_c]) [e1, e2]))

--c)i)Definir Aposta como instância da classe Eq
instance Eq Aposta where
    (==) :: Aposta -> Aposta -> Bool
    (==) a b = comuns a b == (5,2)

--ii)função que dada uma aposta e a chave do concurso, indica qual o prémio que a aposta tem (de acordo com a tabela).
premio :: Aposta -> Aposta -> Maybe Int
premio ap chave =
    case comuns ap chave of 
        (5,e) -> Just (3 - e)
        (4,e) -> Just (6 - e)
        (3,2) -> Just 7
        (3,e) -> Just (10 - e)
        (2,2) -> Just 8
        (2,e) -> Just (13 - e)
        (1,2) -> Just 11
        _ -> Nothing

--d)i)função que lê do teclado uma aposta. Deve garantir que a aposta produzida é válida
leAposta :: IO Aposta
leAposta = do
    putStrLn "Introduz 5 números separados por um espaço:"
    nums <- map read . words <$> getLine :: IO [Int]
    putStrLn "Introduz as 2 estrelas separadas por um espaço:"
    estrelas <- map read . words <$> getLine :: IO [Int]
    if length estrelas /= 2 then
        putStrLn "Aposta invalida!"
        >> leAposta    
    else
        let ap = Ap nums ((\(a:b:_) -> (a,b)) estrelas) in
        if valida ap then
            return ap
        else
            putStrLn "Aposta invalida!"
            >> leAposta

--ii)função que recebe a chave do concurso, lê uma aposta do teclado e imprime o prémio no ecrã
joga :: Aposta -> IO ()
joga chave = do
    ap <- leAposta
    putStrLn $ "Premio: " ++ maybe "sem premio" show (premio ap chave)

--e)função que gera uma chave válida de forma aleatória
geraChave :: IO Aposta
geraChave = do
    nums <- foldr (\_ acc -> do
            prev_nums <- acc
            num <- geraNum (1,50) prev_nums
            return $ num : prev_nums
        ) (return []) [1..5]
    estrela1 <- geraNum (1,12) []
    estrela2 <- geraNum (1,12) [estrela1]
    return $ Ap nums (estrela1, estrela2)

geraNum :: (Int, Int) -> [Int] -> IO Int
geraNum range prev_nums = do
    n <- uniformRM range globalStdGen
    if n `elem` prev_nums then
        geraNum range prev_nums
    else
        return n

--f)função main que permita jogar várias vezes e dê a possiblidade de simular um novo concurso (gerando uma nova chave)
main :: IO ()
main = do
    ch <- geraChave
    ciclo ch

menu :: IO String
menu = do
    putStrLn menutxt
    putStr "Opcao: "
    getLine
    where menutxt = unlines ["",
            "Apostar ........... 1",
            "Gerar nova chave .. 2",
            "",
            "Sair .............. 0"
            ]

ciclo :: Aposta -> IO ()
ciclo chave = do
    opcao <- menu
    case opcao of 
        "1" -> joga chave >> ciclo chave
        "2" -> geraChave >>= (\ch -> putStrLn "Nova chave gerada." >> ciclo ch)
        "0" -> return ()