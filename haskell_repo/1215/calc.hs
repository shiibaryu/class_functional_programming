import Token

calc::[Token]->Int
calc [Num x] = x
calc (Num x:Add:Num y:ts) = calc (Num(x+y):ts)
calc (Num x:Sub:Num y:ts) = calc (Num(x-y):ts)
calc (Num x:Mul:Num y:ts) = calc (Num(x*y):ts)
calc (Num x:Div:Num y:ts) = calc (Num(x `div` y):ts)

main = do cs <- getContents
          putStr $ unlines $ map (show . calc . tokens) $ lines cs
