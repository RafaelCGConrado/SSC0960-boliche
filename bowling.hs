somaBonusStrike :: [Int] -> Int
somaBonusStrike [] = 0
somaBonusStrike (cab:cau) = cab + head cau

somaBonusSpare :: [Int] -> Int
somaBonusSpare [] = 0
somaBonusSpare (cab:cau) = cab

somaPontos :: Int -> [Int] -> Int
somaPontos pontos [] = pontos + 0
somaPontos pontos (cab:cau)
  | cab == 10 = cab + somaBonusStrike cau + somaPontos pontos cau
  | cab + head cau == 10 = cab + head cau + somaBonusSpare (tail cau) + somaPontos pontos (tail cau)
  | otherwise = cab + head cau + somaPontos pontos (tail cau)
  

main :: IO ()
main = do
    putStrLn "Digite o placar de pontos: "
    line <- getLine
    let numbers = map read (words line)

    let pontos = 0

    putStrLn $ "Pontuacao: " ++ show (somaPontos pontos numbers)





    