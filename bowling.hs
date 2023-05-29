-- Funcao que retorna a cabeca de uma lista se a lista nao for vazia:
cabeca :: [Int] -> Int
cabeca [] = 0
cabeca pinos = head pinos

-- Funcao que retorna a cauda de uma lista se a lista nao for vazia:
cauda :: [Int] -> [Int]
cauda [] = []
cauda pinos = tail pinos

-- Funcao de soma do bonus de um strike:
somaBonusStrike :: [Int] -> Int
somaBonusStrike [] = 0 -- Retorna 0 se a lista for vazia
somaBonusStrike (cab:cau)
  | cauda cau == [] = 0 -- Retorna zero se a cauda da cauda for vazia (nao ha bonus no ultimo frame)
  | otherwise = cab + cabeca cau -- Retorna soma da cabeca com a cabeca da cauda da lista (bonus das duas proximas bolas)

-- Funcao de soma do bonus de um spare:
somaBonusSpare :: [Int] -> Int
somaBonusSpare [] = 0 -- Retorna 0 se a lista for vazia
somaBonusSpare (cab:cau)
  | cau == [] = 0 -- Retorna 0 se a cauda da lista for vazia (nao ha bonus no ultimo frame)
  | otherwise = cab -- Retorna a cabeca da lista (bonus da proxima bola)

-- Funcao principal de soma de pontos:
somaPontos :: [Int] -> Int
somaPontos [] = 0 -- Retorna 0 se a lista for vazia
somaPontos (cab:cau)
  | cab == 10 = cab + somaBonusStrike cau + somaPontos cau -- Se for strike, soma a cabeca, com o bonus strike, com a recursao da cauda
  | cab + cabeca cau == 10 = cab + cabeca cau + somaBonusSpare (cauda cau) + somaPontos (cauda cau) -- Se for spare, soma cabeca, com cabeca da cauda, com bonus spare, com a recursao da cauda da cauda
  | otherwise = cab + cabeca cau + somaPontos (cauda cau) -- Se nao for strike nem spare, soma cabeca, com a cabeca da cauda, com a recursao da cauda da cauda

-- Funcao de print dos pontos em cada frame 
printPontos :: [Int] -> IO ()
printPontos [] = return ()  -- Caso base: lista vazia, não faz nada
printPontos (cab:cau)
  | cab == 10 = do -- Strike
    if cauda (cauda cau) == [] then do
      putStr "X " -- Se for um strike no ultimo frame
      if cau /= [] then -- Se nao for a ultima bola
        printPontos cau
      else putStr "" -- Se for a ultima bola
    else do 
      putStr "X _ | " -- Se for um strike que nao eh no ultimo frame
      printPontos cau
      
  | cab + cabeca cau == 10 = do -- Spare
    if cauda (cauda cau) == [] then putStr $ show (cab) ++ " / " -- Se for um spare do ultimo frame
    else putStr $ show (cab) ++ " / | " -- Se for um spare que nao eh do ultimo frame
    printPontos $ cauda cau
    
  | otherwise = do -- Normal
    putStr $ show (cab) ++ " "
    if cau /= [] then do -- Se nao for ultima bola, printa a proxima bola do frame:
      putStr $ show (cabeca cau) ++ " "
      if cauda (cauda cau) /= [] then do -- Se nao for ultimo frame, printa pipe:
        putStr "| "
      else putStr ""
      printPontos $ cauda cau
    else putStr ""

main :: IO ()
main = do
    line <- getLine
    let numbers = map read (words line)

    -- Printa cada frame:
    printPontos numbers
    -- Printa ultimo pipe e a soma dos pontos:
    putStrLn $ "| " ++ show (somaPontos numbers)

    