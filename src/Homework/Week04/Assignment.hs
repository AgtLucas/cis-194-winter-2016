module Homework.Week04.Assignment (
  ex1,
  ex2,
  ex3,
  ex4,
  ex5,
  ex6,
  ex7,
  ex8,
  ex9,
  ex10,
  ex11,
  ex12,
  insertBST,
  allCaps,
  dropTrailingWhitespace,
  firstLetters,
  asList,
  BST(..)
) where

import Homework.Week04.BST
import Data.Char
import Data.List
import Data.Maybe

-- #1
-- a cannot influence b's type, so b = b
ex1 :: a -> b -> b
ex1 _ b = b

-- #2
-- must return either the first or second arg...
ex2 :: a -> a -> a
ex2 a _ = a
--ex2 _ b = b

-- #3
-- Can only return a
ex3 :: Int -> a -> a
ex3 _ x = x

-- #4
-- Finite - Bool could cause return of second or third arg, but thats it
ex4 :: Bool -> a -> a -> a
ex4 True a _ = a
ex4 False _ b = b

-- #5
-- Either id or not based on first arg
-- or const true or false
ex5 :: Bool -> Bool
ex5 = id

-- #6
ex6 :: (a -> a) -> a
ex6 = error "impossible"

-- #7
-- two: return the second arg, or the result of first(second)
ex7 :: (a -> a) -> a -> a
ex7 fn x = fn x
--ex7 _ x = x

-- #8
ex8 :: [a] -> [a]
ex8 = id

-- #9
ex9 :: (a -> b) -> [a] -> [b]
ex9 = map

-- #10
ex10 :: Maybe a -> a
ex10 = error "impossible"
-- fromJust works, but would throw an error when it receices Nothing

-- #11
ex11 :: a -> Maybe a
ex11 = Just

-- #12
ex12 :: Maybe a -> Maybe a
ex12 = id

-- #13
insertBST :: (a -> a -> Ordering) -> a -> BST a -> BST a
insertBST  _ a Leaf = Node Leaf a Leaf
insertBST  order a (Node left root right)
    | order a root == LT = Node (insertBST order a left) root right
    | otherwise = Node right root (insertBST order a right)

-- #14

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x


allCaps :: [String] -> Bool
allCaps = all (maybe False isUpper . safeHead)

-- #15
dropTrailingWhitespace :: String -> String
dropTrailingWhitespace = dropWhileEnd isSpace

-- #16
firstLetters :: [String] -> [Char]
firstLetters = mapMaybe safeHead

-- #17
asList :: [String] -> String
asList xs = concat $ ["["] ++ (intersperse ", " xs) ++ ["]"]
