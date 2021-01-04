import Token

data ParseTree = Number Int | 
                 Plus ParseTree ParseTree
                 Minus ParseTree ParseTree
                 Times ParseTree ParseTree
                 Divide ParseTree ParseTree
                 derving show

type Parser = [Token] -> (ParseTree, [Token])

parseFactor(Num x:ts) = (Number x,ts)

parseTerm | = parseFactor |

parseExpr | = nextTerm $ parseTerm |
  where nextTerm(p1,Add:|1) = let (p2,|2) = parseTerm |1
                              in nextTerm(Plus p1 p2, |2)
        nextTerm(p1,Sub:|1) = let (p2,|2) = parseTerm |1
                              in nextTerm(Minus p1 p2,|2)
        nextTerm x = x
