module BoolLiteral where

--ERROR:
flag = True

--ERROR:
cond x = if x > 0 then True else False

noMatch = False
