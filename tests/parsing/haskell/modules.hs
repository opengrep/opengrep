module Data.Example.Modules
  ( foo
  , bar
  , Baz(..)
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (sort, nub, group)
import Prelude hiding (lookup)

data Baz = BazA | BazB
  deriving (Show, Eq)

foo :: Int -> Int
foo x = x + 1

bar :: String -> String
bar s = s ++ "!"

emptyMap :: Map String Int
emptyMap = Map.empty

sortedUniq :: Ord a => [a] -> [a]
sortedUniq = nub . sort
