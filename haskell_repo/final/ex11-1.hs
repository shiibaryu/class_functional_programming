import System.Environment

div2::Int -> Maybe Int
div2 x = if even x then Just (x `div` 2)
                   else Nothing


main = do print $ div2 2
