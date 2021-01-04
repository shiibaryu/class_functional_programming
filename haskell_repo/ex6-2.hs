import System.Environment

main = do args <- getArgs
          print $ vat $ read $ head args

vat x = truncate (x*1.1 + 0.5)
