module Declarations where

-- Single-clause function
double :: Int -> Int
double x = x * 2

-- Multi-clause function
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- Guarded function
myAbs :: Int -> Int
myAbs x
  | x >= 0    = x
  | otherwise = -x

-- Type signature
myMap :: (a -> b) -> [a] -> [b]
myMap _ []     = []
myMap f (x:xs) = f x : myMap f xs

-- Data type
data Tree a = Leaf a | Node (Tree a) (Tree a)

-- Newtype
newtype Wrapper a = Wrapper { unwrap :: a }

-- Type alias
type Name = String

-- Pattern synonym
pattern Empty :: [a]
pattern Empty = []

-- Fixity declaration
infixl 6 `myAdd`

myAdd :: Int -> Int -> Int
myAdd x y = x + y
