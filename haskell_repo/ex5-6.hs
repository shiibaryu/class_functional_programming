import System.Environment

main = do args <- getArgs
          print $ factors $ read $ head args

factor n = filter divisible [1..n]
  where divisible m = n "mod" m == 0
