module DeepExpr where

main = do
--ERROR:
  foo 1
  bar 1

other = do
  foo 1
  bar 1
  baz  -- extra stmt: pattern (do; foo 1; bar 1) doesn't match a 3-stmt do

different = do
  foo 1
  bar 2  -- different arg, should not match
