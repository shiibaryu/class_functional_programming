import System.Environment

main = do args <- getArgs
          print $ r2a $ head args

--I 1,V 5, X 10, L 50, C 100, D 500, M 1000
romain::Char->Int
romain 'I' = 1
romain 'V' = 5
romain 'X' = 10
romain 'L' = 50
romain 'C' = 100
romain 'D' = 500
romain 'M' = 1000

r2a::String -> Int
r2a a = check $ map romain a

check::[Int] -> Int
check = fst . foldr (\x (c,h) -> if x >=  h then (c + x,x) else (c - x,x)) (0,0)

