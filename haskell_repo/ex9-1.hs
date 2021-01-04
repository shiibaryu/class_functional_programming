import System.Environment
import Data.Ratio

data Rat = Rat Integer Integer

instance Show Rat where
  show (Rat x y) | x == 0 = show 0
                 | y == 1 = show x
                 | otherwise = show x ++ "/" ++ show y

comp::Rat -> Rat
comp (Rat x y) | x `mod` 2 == 0 && y `mod` 2 == 0 = comp (Rat (x`div`2) (y`div`2))
               | x `mod` 3 == 0 && y `mod` 3 == 0 = comp (Rat (x`div`3) (y`div`3))
               | x `mod` 5 == 0 && y `mod` 5 == 0 = comp (Rat (x`div`5) (y`div`5))
               | x `mod` 7 == 0 && y `mod` 7 == 0 = comp (Rat (x`div`7) (y`div`7))
               | y < 0 = Rat (-x) (-y)
               | otherwise = Rat x y

instance Num Rat where
  (Rat x y) + (Rat u v) = comp(Rat (x * v + u * y) (y*v))
  (Rat x y) - (Rat u v) = comp(Rat (x * v - u * y) (y*v))
  (Rat x y) * (Rat u v) = comp(Rat (x*u) (y*v))
  negate (Rat x y)      = Rat (-x) y
  abs (Rat x y)         = Rat (abs x) (abs y)
  signum (Rat x y)      | x == 0 = fromInteger 0
                        | x * y > 0 = fromInteger 1
                        | otherwise = -1
  fromInteger x         = Rat x 1
 
main = do args <- getArgs
          let x = read (args !! 0)
          let y = read (args !! 1)
          let u = read (args !! 2)
          let v = read (args !! 3)
          print $ (Rat x y + Rat u v)
          print $ (Rat x y - Rat u v)
          print $ (Rat x y * Rat u v)
