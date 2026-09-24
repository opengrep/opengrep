module FnOtherwise where


f x
  | x > 0 = "pos"
--ERROR:
  | otherwise = "neg"

other = 0
