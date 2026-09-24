module LetNested where


--ERROR:
f = let a = let b = 1 in b + 1 in a * 2

other = 0
