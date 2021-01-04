main = do cs <- getContents
          putStr $ firstNLines 10 cs
