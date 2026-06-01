module MetaEqExpr where

--ERROR:
selfEq x = x == x

--ERROR:
badCheck = True == True

notSelf x y = x == y
