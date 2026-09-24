module LetMulti where


--ERROR:
f a = let x = a + 1
          y = a - 1 in x * y

other = 0
