{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}
module Calc where
import Control.Monad
import qualified Data.Map as M

import qualified ExprT as E
import qualified StackVM as S
import qualified VarExprT as V
import Parser

-- Exercise 1
eval :: E.ExprT -> Integer
eval (E.Lit e)     = e
eval (E.Add e1 e2) = eval e1 + eval e2
eval (E.Mul e1 e2) = eval e1 * eval e2

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp E.Lit E.Add E.Mul

-- Exercise 3
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr E.ExprT where
  lit = E.Lit
  add = E.Add
  mul = E.Mul

reify :: E.ExprT -> E.ExprT
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

-- Exercise 5
instance Expr S.Program where
  lit     = (:[]) . S.PushI
  add x y = x ++ y ++ [S.Add]
  mul x y = x ++ y ++ [S.Mul]

compile :: String -> Maybe S.Program
compile = parseExp lit add mul

-- Exercise 6
class HasVars a where
  var :: String -> a

instance Expr V.VarExprT where
  lit = V.Lit
  add = V.Add
  mul = V.Mul

instance HasVars V.VarExprT where
  var = V.Var

type Env = M.Map String Integer

instance HasVars (Env -> Maybe Integer) where
  var = M.lookup

instance Expr (Env -> Maybe Integer) where
  lit x   e = Just x
  add a b e = liftM2 (+) (a e) (b e)
  mul a b e = liftM2 (*) (a e) (b e)

withVars :: [(String, Integer)] -> (Env -> Maybe Integer) -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
