{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UnboxedTuples #-}
module AdvancedTypes where

-- GADT
data Expr a where
  LitInt  :: Int  -> Expr Int
  LitBool :: Bool -> Expr Bool
  Add     :: Expr Int -> Expr Int -> Expr Int
  If      :: Expr Bool -> Expr a -> Expr a -> Expr a

evalExpr :: Expr a -> a
evalExpr (LitInt n)    = n
evalExpr (LitBool b)   = b
evalExpr (Add x y)     = evalExpr x + evalExpr y
evalExpr (If c t e)    = if evalExpr c then evalExpr t else evalExpr e

-- Open type family
type family Repr a :: *
type instance Repr Int  = Integer
type instance Repr Bool = Bool

-- Closed type family
type family IsInt a where
  IsInt Int = True
  IsInt _   = False

-- Data family
data family Vector a
data instance Vector Int  = VInt  [Int]
data instance Vector Bool = VBool [Bool]

-- Role annotations
type role Map nominal representational

-- Unboxed tuple
swapUnboxed :: (# Int, Bool #) -> (# Bool, Int #)
swapUnboxed (# x, y #) = (# y, x #)
