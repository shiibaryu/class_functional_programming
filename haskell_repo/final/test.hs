import System.Environment

main = do cs <- getArgs
          as <- getContents
	  putStr $ unlines cs
	  putStr as
