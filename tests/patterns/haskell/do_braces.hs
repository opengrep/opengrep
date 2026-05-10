module DoBraces where

--ERROR:
compactDo = do { x <- getLine; putStrLn x }

--ERROR:
compactDo2 = do { input <- readFile "f"; print input }

-- This also matches because the AST unifies brace and layout syntax
--ERROR:
regularDo = do
  x <- getLine
  putStrLn x
