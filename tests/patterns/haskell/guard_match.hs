module GuardMatch where

classify n
--ERROR:
  | n < 0 = "neg"
  | otherwise = "non-neg"

isNeg x
--ERROR:
  | x < 0 = True
  | otherwise = False

noGuard n = n + 1
