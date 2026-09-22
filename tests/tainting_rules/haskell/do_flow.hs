module DoFlow where

unsafe = do
  x <- getUserInput
  --ruleid: test-do-flow
  writeFile "log.txt" x

--OK:
safe = do
  x <- return "safe"
  writeFile "log.txt" x
