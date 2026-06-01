module Guards where

-- Boolean guards
bmi :: Double -> String
bmi b
  | b < 18.5  = "Underweight"
  | b < 25.0  = "Normal"
  | b < 30.0  = "Overweight"
  | otherwise = "Obese"

-- Guards with where clause
bmiCategory :: Double -> Double -> String
bmiCategory weight height
  | b < 18.5  = "Underweight"
  | b < 25.0  = "Normal"
  | b < 30.0  = "Overweight"
  | otherwise = "Obese"
  where
    b = weight / height ^ 2

-- Pattern guards
describeList :: Show a => [a] -> String
describeList xs
  | null xs        = "empty"
  | [_] <- xs      = "singleton"
  | length xs < 5  = "short"
  | otherwise      = "long"

-- Multi-clause with guards
fizzbuzz :: Int -> String
fizzbuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 3  == 0 = "Fizz"
  | n `mod` 5  == 0 = "Buzz"
  | otherwise       = show n

-- Guards in case expression
classify :: Int -> String
classify n = case n of
  0           -> "zero"
  x | x < 0  -> "negative"
    | x < 10 -> "small"
    | otherwise -> "large"
