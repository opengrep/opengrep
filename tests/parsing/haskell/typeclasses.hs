module Typeclasses where

-- Class declaration
class MyEq a where
  myEq :: a -> a -> Bool
  myNeq :: a -> a -> Bool
  myNeq x y = not (myEq x y)

-- Instance declaration
instance MyEq Bool where
  myEq True  True  = True
  myEq False False = True
  myEq _     _     = False

instance MyEq Int where
  myEq x y = x == y

-- Deriving stock
data Color = Red | Green | Blue
  deriving (Show, Eq, Ord)

-- Newtype with deriving newtype
newtype Age = Age Int
  deriving newtype (Num, Eq, Ord)

-- Class with associated type
class Container f where
  type Elem f :: *
  empty :: f
  insert :: Elem f -> f -> f

-- Class with default methods
class Printable a where
  display :: a -> String
  display = const "<unprintable>"

  displayLn :: a -> String
  displayLn x = display x ++ "\n"
