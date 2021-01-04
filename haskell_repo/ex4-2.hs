import System.Environment
import Data.List

main = do args <- getArgs
          cs   <- getContents
          putStr $ fgrep (head args) cs

fgrep :: String -> String -> String
fgrep pattern cs = unlines $ filter match $ lines cs
  where
    match :: String -> Bool  
    match line = any prefixp $ tails line 

    prefixp :: String -> Bool
    prefixp line = (map lower pattern)  `isPrefixOf` (map lower line)

lower :: Char -> Char
lower 'A' = 'a'
lower 'B' = 'b'
lower 'C' = 'c'
lower 'D' = 'd'
lower 'E' = 'e'
lower 'F' = 'f'
lower 'G' = 'g'
lower 'H' = 'h'
lower 'I' = 'i'
lower 'J' = 'j'
lower 'K' = 'k'
lower 'L' = 'l'
lower 'M' = 'm'
lower 'N' = 'n'
lower 'O' = 'o'
lower 'P' = 'o'
lower 'Q' = 'o'
lower 'R' = 'o'
lower 'S' = 'o'
lower 'T' = 'o'
lower 'U' = 'o'
lower 'V' = 'o'
lower 'W' = 'o'
lower 'X' = 'o'
lower 'Z' = 'o'
lower c = c
