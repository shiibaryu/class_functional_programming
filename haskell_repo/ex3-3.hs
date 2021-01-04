main = do cs <- getContents
          putStr $ concatMap tranSfc cs

tranSfc :: Char -> String
tranSfc 'S' = "Shonan "
tranSfc 'F' = "Fujisawa "
tranSfc 'C' = "Campus "
tranSfc c = [c]
