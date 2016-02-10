module Homework.Week05.Assignment (
  eval,
  evalStr,
  ExprT(..),
  Expr(..),
  MinMax(..),
  Mod7(..)
) where

import Homework.Week05.ExprT
import Homework.Week05.Parser

-- #1
eval :: ExprT -> Integer
eval (Lit a) = a
eval (Mul a b) = (eval a) * (eval b)
eval (Add a b) = (eval a) + (eval b)

-- #2
evalStr :: String -> Maybe Integer
evalStr str = case (parseExp Lit Add Mul str) of
    (Just x) -> Just (eval x)
    Nothing  -> Nothing

-- #3
class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

-- #4
instance Expr Integer where
    lit x = eval (Lit x)
    add x y = eval (Add (Lit x) (Lit y))
    mul x y = eval (Mul (Lit x) (Lit y))

instance Expr Bool where
    lit x
        | x <= 0 = False
        | otherwise = True
    add x y = x || y
    mul x y = x && y

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
    lit = MinMax
    add (MinMax x) (MinMax y) = MinMax (max x y)
    mul (MinMax x) (MinMax y) = MinMax (min x y)

instance Expr Mod7 where
    lit x = Mod7 $ x `mod` 7
    add (Mod7 x) (Mod7 y) = Mod7 $ (x + y) `mod` 7
    mul (Mod7 x) (Mod7 y) = Mod7 $ (x * y) `mod` 7
