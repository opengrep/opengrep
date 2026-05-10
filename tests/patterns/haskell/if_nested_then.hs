module IfNested where


--ERROR:
f a b = if a then if b then 1 else 2 else 3

other = 0
