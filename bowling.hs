somaPontos [] = 0 
somaPontos (h:t) = do
  -- |h == 10 = h + head t + head $ tail t
  |h == 10 = h + somaPontos t
  
-- soma (h:t) = do
--  | h 10 = asihuifh(soma)
--  | h != 10 ouihaiuhc(soma)

main :: IO ()
main = do
    putStrLn "Digite o placar de pontos: "
    line <- getLine
    let numbers = map read (words line) :: [Integer]
    putStrLn $ "Primeiro elemento: " ++ show (head (tail numbers))

