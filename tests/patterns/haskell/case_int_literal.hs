module CaseInt where


--ERROR:
f x = case x of
  0 -> "zero"
  _ -> "other"
