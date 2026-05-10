module LetBinding where

--ERROR:
result = let x = 42 in x + 1

--ERROR:
other = let y = "hello" in y

noLet z = z
