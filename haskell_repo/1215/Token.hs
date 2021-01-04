--module Token(Token(..),tokens) where
import Data.Char

data Token = Num Int | Add | Sub | Mul | Div | Lpar | Rpar 

data ParseTree = Number Int | 
                 Plus ParseTree ParseTree
                 Minus ParseTree ParseTree
                 Times ParseTree ParseTree
                 Divide ParseTree ParseTree
                 deriving Show

type Parser = [Token] -> (ParseTree, [Token])

tokens::String -> [Token]
tokens [] = []
tokens ('+':cs) = Add:(tokens cs)
tokens ('-':cs) = Sub:(tokens cs)
tokens ('*':cs) = Mul:(tokens cs)
tokens ('/':cs) = Div:(tokens cs)
tokens ('(':cs) = Lpar:(tokens cs)
tokens (')':cs) = Rpar:(tokens cs)
tokens (c:cs)   | isDigit c = let (ds,rs) = span isDigit (c:cs)
                               in Num(read ds):(tokens rs)
                | otherwise  = tokens cs

parseFactor(Num x:ts) = (Number x,ts)
parseTerm l = parseFactor l
parseExpr l = nextTerm $ parseTerm l
  where nextTerm(p1,Add:l1) = let (p2,l2) = parseTerm l1
                              in nextTerm(Plus p1 p2, l2)
        nextTerm(p1,Sub:l1) = let (p2,l2) = parseTerm l1
                              in nextTerm(Minus p1 p2,l2)
        nextTerm x = x

eval::parseTree -> Int
eval(Number x) = x
eval(Plus p1 p2) = eval p1 + eval p2
eval(Minus p1 p2) = eval p1 - eval p2
eval(Time p1 p2) = eval p1 * eval p2
eval(Devide p1 p2) = eval p1 `div` eval p2

main = do cs <- getContents
          putStr $ unlines $ map (show . eval . fst . parseExpr . tokens) $ lines cs
