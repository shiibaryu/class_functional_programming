import System.Environment
import Data.List

main = print $ take 20 $ odds 1
odds n = n : odds(n+2)
