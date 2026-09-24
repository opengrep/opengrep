module CaseMaybe where


--ERROR:
go m = case m of
  Just y -> y
  Nothing -> 0
