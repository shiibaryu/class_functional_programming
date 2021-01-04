main = do cs <- getContents
          putStr $ firstNLines 5 $ lines cs
          putStrLn "..."
          putStr $ lastNLines 5 $ lines cs

firstNLines n cs = unlines $ take n $ cs
lastNLines  n cs = unlines $ reverse $ take n $ reverse $ cs
