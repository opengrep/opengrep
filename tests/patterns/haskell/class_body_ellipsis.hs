module ClassBodyEllipsis where

--ERROR:
class Pretty a where
  pretty :: a -> String

--ERROR:
class Container f where
  empty :: f a
  insert :: a -> f a -> f a

data NotAClass = NotAClass
