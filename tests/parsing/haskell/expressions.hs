module Expressions where

-- Function application
appSimple :: Int
appSimple = negate 5

appMulti :: [Int]
appMulti = map (+1) [1, 2, 3]

-- Infix operators
infixPlus :: Int
infixPlus = 3 + 4

infixDiv :: Int
infixDiv = 10 `div` 3

infixAppend :: [Int]
infixAppend = [1, 2] ++ [3, 4]

-- $ operator
dollarOp :: IO ()
dollarOp = putStrLn $ show 42

-- Negation
negation :: Int
negation = negate (-5)

-- Tuples
tuple3 :: (Int, String, Bool)
tuple3 = (1, "hello", True)

-- Lists
listLit :: [Int]
listLit = [1, 2, 3]

emptyList :: [Int]
emptyList = []

-- Arithmetic sequences
seqFull :: [Int]
seqFull = [1..10]

seqStep :: [Int]
seqStep = [1,3..10]

-- If-then-else
absVal :: Int -> Int
absVal x = if x > 0 then x else -x

-- Case-of
headOrZero :: [Int] -> Int
headOrZero xs = case xs of
  []    -> 0
  (x:_) -> x

-- Lambda
addOne :: Int -> Int
addOne = \x -> x + 1

addTwo :: Int -> Int -> Int
addTwo = \x y -> x + y

-- Let-in
letIn :: Int
letIn = let x = 1 in x + 2

-- Where
whereClause :: Int -> Int
whereClause x = y + 1
  where y = x * 2

-- List comprehension
evens :: [Int]
evens = [x * 2 | x <- [1..10], even x]

-- Operator sections
addOneSection :: Int -> Int
addOneSection = (+1)

doubleSection :: Int -> Int
doubleSection = (2*)

-- Type annotation
annotated :: Int
annotated = (42 :: Int)

-- Multi-way if
{-# LANGUAGE MultiWayIf #-}
sign :: Int -> Int
sign x = if | x > 0    -> 1
             | x == 0   -> 0
             | otherwise -> -1
