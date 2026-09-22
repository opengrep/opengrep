module MultiClauseMeta where

fact :: Int -> Int
--ERROR:
fact 0 = 1
--ERROR:
fact n = n * fact (n - 1)

other 0 = 5
