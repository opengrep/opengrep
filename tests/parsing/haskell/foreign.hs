module Foreign where

foreign import ccall "math.h sin"
  c_sin :: Double -> Double

foreign import ccall "math.h cos"
  c_cos :: Double -> Double

foreign import ccall unsafe "string.h strlen"
  c_strlen :: Ptr Char -> IO Int

foreign export ccall
  addInt :: Int -> Int -> Int

addInt :: Int -> Int -> Int
addInt x y = x + y
