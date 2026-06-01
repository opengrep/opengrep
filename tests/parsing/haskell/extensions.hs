{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
module Extensions where

import Data.String (IsString)

-- LambdaCase
describeNum :: Int -> String
describeNum = \case
  0 -> "zero"
  1 -> "one"
  _ -> "many"

-- MultiWayIf
classify :: Int -> String
classify n = if | n < 0     -> "negative"
                | n == 0    -> "zero"
                | n < 100   -> "small"
                | otherwise -> "large"

-- OverloadedStrings
greeting :: IsString s => s
greeting = "Hello, World!"

-- ScopedTypeVariables
scopedExample :: forall a. [a] -> Int
scopedExample (xs :: [a]) = length xs

-- TupleSections
pairWithOne :: [a] -> [(Int, a)]
pairWithOne = map (1,)
