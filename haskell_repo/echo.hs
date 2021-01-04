import System.Environment

main = do args <- getArgs
          putStr $ unwords args
