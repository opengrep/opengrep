This is a literate Haskell file. Prose lines are ignored by the
compiler; only the lines starting with a bird track are code.

> module Literate where
>
> import Data.List (sort)

The main function sorts its input.

> main :: IO ()
> main = do
>   xs <- fmap lines getContents
>   mapM_ putStrLn (sort xs)
