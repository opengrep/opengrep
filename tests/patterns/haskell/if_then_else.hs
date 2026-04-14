module IfThenElse where

--ERROR:
absVal x = if x > 0 then x else negate x

--ERROR:
pick b = if b then "yes" else "no"

noIf x = x + 1
