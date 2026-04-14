module DoNotation where

import System.IO

-- Basic do-block
greet :: IO ()
greet = do
  line <- getLine
  putStrLn line

-- Let in do
printAnswer :: IO ()
printAnswer = do
  let x = 42
  print x

-- Sequence
twoOutputs :: IO ()
twoOutputs = do
  putStr "a"
  putStr "b"
  putStrLn ""

-- Nested do
nested :: IO ()
nested = do
  result <- do
    line <- getLine
    return (length line)
  print result

-- Monadic bind and sequencing
readAndEcho :: IO ()
readAndEcho = do
  contents <- readFile "/dev/null"
  putStr contents

-- do with multiple binds and lets
pipeline :: IO ()
pipeline = do
  line  <- getLine
  let upper = map id line
  let n     = length upper
  putStrLn $ "Length: " ++ show n
