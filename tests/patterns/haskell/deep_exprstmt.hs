module DeepExpr where

--ERROR:
main = foo 1

bar = do
--ERROR:
  foo 1
  baz 2
