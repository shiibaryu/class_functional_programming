import System.Environment

main = do args <- getArgs
          print $ sum $ map read args
