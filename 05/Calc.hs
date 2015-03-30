{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}
module Calc where
import Control.Monad

import ExprT
import Parser

-- Exercise 1
eval :: ExprT -> Integer
eval (Lit e)     = e
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul

-- Exercise 3
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

reify :: ExprT -> ExprT
reify = id

-- Exercise 4
instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (> 0)
  add = (||)
  mul = (&&)

newtype MinMax a = MinMax a deriving (Show, Eq)
newtype Mod7   a = Mod7   a deriving (Show, Eq)

instance Monad MinMax where
  return         = MinMax
  MinMax a >>= f = f a

instance Monad Mod7 where
  return       = Mod7
  Mod7 a >>= f = f a

instance Expr (MinMax Integer) where
  lit = return
  add = liftM2 max
  mul = liftM2 min

instance Expr (Mod7 Integer) where
  lit     = return . (`mod` 7)
  add a b = liftM2 (+) a b >>= lit
  mul a b = liftM2 (*) a b >>= lit

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool    = testExp :: Maybe Bool
testMM      = testExp :: Maybe (MinMax Integer)
testSat     = testExp :: Maybe (Mod7 Integer)
