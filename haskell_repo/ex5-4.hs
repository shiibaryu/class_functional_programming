import System.Environment
import Data.List

main = do args <- getArgs
          print $ fact $ read $ head args

fact 0 = 1
fact n = n * fact(n-1)
