cabeca :: [Int] -> Int
cabeca [] = 0
cabeca pinos = head pinos

cauda :: [Int] -> [Int]
cauda [] = []
cauda pinos = tail pinos

somaBonusStrike :: [Int] -> Int
somaBonusStrike [] = 0
somaBonusStrike (cab:cau)
  | cauda cau == [] = 0
  | otherwise = cab + cabeca cau

somaBonusSpare :: [Int] -> Int
somaBonusSpare [] = 0
somaBonusSpare (cab:cau)
  | cau == [] = 0 
  | cauda cau == [] = cab
  | otherwise = cab

somaPontos :: [Int] -> Int
somaPontos [] = 0
somaPontos (cab:cau)
  | cab == 10 = cab + somaBonusStrike cau + somaPontos cau
  | cab + cabeca cau == 10 = cab + cabeca cau + somaBonusSpare (cauda cau) + somaPontos (cauda cau)
  | otherwise = cab + cabeca cau + somaPontos (cauda cau)


printPontos :: [Int] -> IO ()
printPontos [] = return ()  -- Caso base: lista vazia, não faz nada
printPontos (cab:cau)
  | cab == 10 = do -- Strike
    if cauda (cauda cau) == [] then do
      putStr "X "
      if cau /= [] then
        printPontos cau
      else putStr ""   
    else do 
      putStr "X _ | " 
      printPontos cau
      
  | cab + cabeca cau == 10 = do -- Spare
    if cauda (cauda cau) == [] then putStr $ show (cab) ++ " / "
    else putStr $ show (cab) ++ " / | "
    printPontos $ cauda cau
    
  | otherwise = do -- Normal
    putStr $ show (cab) ++ " "
    if cau /= [] then do 
      putStr $ show (cabeca cau) ++ " "
      if cauda (cauda cau) /= [] then do
        putStr "| "
      else putStr ""
      printPontos $ cauda cau
    else putStr ""

main :: IO ()
main = do
    line <- getLine
    let numbers = map read (words line)

    printPontos numbers
    putStrLn $ "| " ++ show (somaPontos numbers)

    