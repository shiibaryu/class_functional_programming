
main = print $ take 20 $ filter difTwo $ twin $ filter isPrime [3..]

isPrime n = if n > 1 then null [ x | x <- [2..n - 1], n `mod`  x == 0] else False

-- 差を取って, 2ならペアを作る
-- twin tail xs
--
twin :: [Int] -> [(Int,Int)]
twin xs = zip xs (tail xs)

difTwo (x,y) = y -x == 2
