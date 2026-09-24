module MultiClauseLiteral where

--ERROR:
fact 0 = 1
fact n = n * fact (n - 1)

other 0 = 5
other n = n
