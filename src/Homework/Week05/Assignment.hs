module Homework.Week05.Assignment (
  eval,
  evalStr,
  ExprT(..)
) where

import Homework.Week05.ExprT
import Homework.Week05.Parser

-- #1
eval :: ExprT -> Integer
eval = undefined

-- #2
evalStr :: String -> Maybe Integer
evalStr = undefined

-- #3
-- class Expr a where 
--   lit :: ???
--   add :: ???
--   mul :: ???

-- #4
-- instance Integer Expr where
--   lit = ???
--   add = ???
--   mul = ???
