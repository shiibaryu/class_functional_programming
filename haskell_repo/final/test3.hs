import Data.List.Split

takeLast n ss = reverse $ take n $ reverse ss

main = do cs <- getContents
          let a  = splitOn " " cs
	  let first = (take 3 a) !! 0
	  let snd = (take 3 a) !! 1
          let th = (take 3 a) !! 2
	  --let first = reverse $ drop 1 $ reverse $ unlines $ take 1 a

	  putStr  snd
          --let b = unlines $ tail $ take 2 $ splitOn " " cs
          --let c = unlines $ tail $ take 3 $ splitOn " " cs
--	  putStr a
--	  putStr b
--	  putStr c
