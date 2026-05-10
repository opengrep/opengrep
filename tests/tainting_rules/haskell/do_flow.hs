module DoFlow where

--ruleid: test-do-flow
unsafe = do
  x <- getUserInput
  writeFile "log.txt" x

--OK:
safe = do
  x <- return "safe"
  writeFile "log.txt" x
