module CaseExpr where

--ERROR:
describeList xs = case xs of
  [] -> "empty"
  _  -> "non-empty"

notACase x = x + 1
