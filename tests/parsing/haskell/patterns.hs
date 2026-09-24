module Patterns where

-- Variable pattern
identity :: a -> a
identity x = x

-- Wildcard pattern
ignoreArg :: a -> Int
ignoreArg _ = 0

-- Constructor pattern
fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing  = error "fromJust: Nothing"

-- As-pattern
firstAndAll :: [a] -> ([a], a)
firstAndAll xs@(x:_) = (xs, x)
firstAndAll []        = error "empty list"

-- List cons pattern
splitHead :: [a] -> (a, [a])
splitHead (x:xs) = (x, xs)
splitHead []     = error "empty list"

-- List literal patterns
singletonHead :: [a] -> a
singletonHead [x] = x
singletonHead _   = error "not singleton"

pairSum :: [Int] -> Int
pairSum [x, y] = x + y
pairSum _      = 0

-- Literal pattern
isZero :: Int -> Bool
isZero 0 = True
isZero _ = False

-- Nested pattern
deepHead :: Maybe (Either b [a]) -> a
deepHead (Just (Right (x:_))) = x
deepHead _                    = error "no match"
