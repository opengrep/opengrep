module Literals where

-- Integer literals
decimalInt :: Int
decimalInt = 42

hexInt :: Int
hexInt = 0xFF

octalInt :: Int
octalInt = 0o77

binaryInt :: Int
binaryInt = 0b1010

-- Float literals
plainFloat :: Double
plainFloat = 3.14

expFloat :: Double
expFloat = 1e10

negExpFloat :: Double
negExpFloat = 2.5e-3

-- Char literals
plainChar :: Char
plainChar = 'a'

newlineChar :: Char
newlineChar = '\n'

hexChar :: Char
hexChar = '\x41'

-- String literals
plainString :: String
plainString = "hello"

escapeString :: String
escapeString = "line1\nline2"
