--module Token(Token(..),tokens) where

import Data.Char
import Control.Applicative

-- modules imported for ex11-6
import System.Environment
import Network.Socket 
import Control.Concurrent
import Network.Socket.ByteString (recv,sendAll) 
import qualified Data.ByteString.Char8 as B8
--import qualified Data.ByteString as S
import Data.List.Split
--

-- functions implemented for ex11-6 ---------------------------------------------------------

takeLast n ss = reverse $ take n $ reverse ss

getClientAddr hints addr port = getAddrInfo (Just hints) (Just addr) (Just port)

getServerAddr hints host port = getAddrInfo (Just hints) host (Just port)

makeSocket addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)

runClient s_addr port calc = withSocketsDo $ do 
                   let hints = defaultHints {addrSocketType = Stream}
                   addr <- head <$> getClientAddr hints s_addr port
                   sock <- makeSocket addr
                   connect sock $ addrAddress addr
                   msgSend sock calc
                   close sock

msgSend sock msg = do 
          sendAll sock $ B8.pack msg
          res <- Network.Socket.ByteString.recv sock 1000
          B8.putStrLn res

runServer port = withSocketsDo $ do
                   sock <- socket AF_INET Stream defaultProtocol
                   bind sock (SockAddrInet port  0)
                   listen sock 10
                   waitConnection sock
                   close sock

waitConnection sock = do
          (conn,_) <- accept sock
          forkIO $ rcvMsg conn
          --waitConnection sock

netProcess cs = show $ eval $ fst $ parseExpr $ tokens cs

charToString c = [c]

--doCalc cs =  putStr $ unlines $ map process $ lines cs
rcvMsg conn = do 
          msg <- Network.Socket.recv conn 100
          --msg <- Network.Socket.ByteString.recv conn 1000
          --B8.putStrLn msg
          let res = netProcess msg
          --let res = unlines $ map netProcess $ lines msg
          --res <- map netProcess $ lines msg
          sendAll conn $ B8.pack $ (charToString res) !! 0

--------------------------------------------------------------------------------------

data MayError a = Value a | Error String

instance (Show a) => Show (MayError a) where
  show (Value x) = show x
  show (Error s) = "error: " ++ s

instance Functor MayError where
  fmap f (Value x) = Value (f x)
  fmap f (Error s) = Error s

instance Applicative MayError where
  pure x = Value x
  (Value f) <*> (Value x) = Value (f x)
  (Value f) <*> (Error s) = Error s
  (Error s) <*> _ = Error s

instance Monad MayError where
  return x = Value x
  (Value x) >>= f = f x
  (Error s) >>= f = Error s

{- tokenizer -}
data Token = Num Int | Add | Sub | Mul | Div | LPar | RPar deriving (Eq, Show)

tokens::String -> [Token]
tokens [] = []
tokens ('+':cs) = Add:(tokens cs)
tokens ('-':cs) = Sub:(tokens cs)
tokens ('*':cs) = Mul:(tokens cs)
tokens ('/':cs) = Div:(tokens cs)
tokens ('(':cs) = LPar:(tokens cs)
tokens (')':cs) = RPar:(tokens cs)
tokens (c:cs)   | isDigit c = let (ds,rs) = span isDigit (c:cs)
                               in Num(read ds):(tokens rs)
                | isSpace c = tokens cs
                | otherwise = tokens cs

{-parser-}
data ParseTree = Number Int | 
                 Plus ParseTree ParseTree |
                 Minus ParseTree ParseTree |
                 Times ParseTree ParseTree |
                 Divide ParseTree ParseTree 
                 deriving Show

type Parser = [Token] -> (ParseTree, [Token])

parseFactor::Parser
parseFactor(Num x:ts) = (Number x,ts)
parseFactor(Add:ts) = parseFactor ts
parseFactor(Sub:ts) = let (p1,ts1) = parseFactor ts in (Minus (Number 0) p1,ts1)
parseFactor(LPar:ts) = let (p1,RPar:ts) = parseExpr ts in (p1,ts)

parseTerm::Parser
parseTerm l = nextFactor $ parseFactor l
  where nextFactor(p1,Mul:l1) = let (p2,l2) = parseFactor l1
                                in nextFactor(Times p1 p2, l2)
        nextFactor(p1,Div:l1) = let (p2,l2) = parseFactor l1
                                in nextFactor(Divide p1 p2, l2)
        nextFactor x = x

parseExpr::Parser
parseExpr l = nextTerm $ parseTerm l
  where nextTerm(p1,Add:l1) = let (p2,l2) = parseTerm l1
                              in nextTerm(Plus p1 p2, l2)
        nextTerm(p1,Sub:l1) = let (p2,l2) = parseTerm l1
                              in nextTerm(Minus p1 p2,l2)
        nextTerm x = x

{-evaluator-}
eval::ParseTree -> MayError Int
eval(Number x) = Value x
eval(Plus p1 p2) = do x <- eval p1
                      y <- eval p2
                      return (x + y)
eval(Minus p1 p2) = do x <- eval p1
                       y <- eval p2
                       return (x - y)
eval(Times p1 p2) = do x <- eval p1
                       y <- eval p2
                       return (x * y)
eval(Divide p1 p2) = do x <- eval p1
                        y <- eval p2
                        if y == 0 then Error "division by 0"
                                  else return (x `div` y)

doCalc cs =  putStr $ unlines $ map process $ lines cs
             where process cs = show $ eval $ fst $ parseExpr $ tokens cs

doNetCalc cs = do let cp = cs
                  let as = take 4 $ splitOn " " cp
                  let net = as !! 0
                  let port = as !! 1
                  let addr = as !! 2
                  let calc = as !! 3
                  if net == "net_cli" then runClient addr port calc
                                      else if net == "net_ser" then runServer $ read port
                                      else putStr "error"

main = do args <- getArgs
          cs   <- getContents
          let mode = read $ args !! 0
          --m <- toInt mode
          if mode == 1 then doCalc cs 
                       else if mode == 2 then doNetCalc cs
                       else putStr "error"
