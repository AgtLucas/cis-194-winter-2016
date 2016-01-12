module Homework.Week01.Assignment where

-- #1a
toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits x
    | x < 0     = []
    | otherwise = map (\n -> read [n]::Integer) (show x)

-- #1b
toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

-- #2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleLeftEveryOther . reverse

-- double every other number starting from the left
-- e.g. [1,2,3,4] -> [1,4,3,8]
doubleLeftEveryOther :: [Integer] -> [Integer]
doubleLeftEveryOther [] = []
doubleLeftEveryOther (x:[]) = [x]
doubleLeftEveryOther (x:y:zs) = x : (y*2) : (doubleLeftEveryOther zs)

-- #3
sumDigits :: [Integer] -> Integer
sumDigits x = foldl (+) 0 (singleDigits x)

-- make sure we have a list with a single digits
-- e.g. [1,32,4] -> [1,3,2,4]
singleDigits :: [Integer] -> [Integer]
singleDigits x = concat (map toDigits x)

-- #4
validate :: Integer -> Bool
validate x =
    let sum = (sumDigits . doubleEveryOther . toDigits) x
    in sum `mod` 10 == 0

-- #5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b _ = [(a, b)]
hanoi d a b c = (hanoi (d-1) a c b) ++ [(a, b)] ++ (hanoi (d-1) c b a)

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 = undefined
