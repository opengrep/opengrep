module Types where

-- Function type
myFunc :: Int -> Int -> Bool
myFunc x y = x < y

-- Type application
maybeInt :: Maybe Int
maybeInt = Just 42

eitherVal :: Either String Int
eitherVal = Right 1

-- Tuple type
triple :: (Int, String, Bool)
triple = (1, "hi", True)

-- List type
ints :: [Int]
ints = [1, 2, 3]

-- Constrained polymorphism
myMin :: Ord a => a -> a -> a
myMin x y = if x <= y then x else y

showAndCompare :: (Ord a, Show a) => a -> String
showAndCompare x = show x

-- Higher-kinded type
class Functor' f where
  fmap' :: (a -> b) -> f a -> f b

-- Type family
type family F (a :: *) :: *
type instance F Int = Bool
type instance F Bool = Int

-- Kind signature in class
class MyClass (f :: * -> *) where
  method :: f Int -> Int
