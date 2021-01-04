main = do cs <- getContents
       putStr $ unlines $ map addTap $ lines cs
       

addTap cs = "\tab" ++ cs
