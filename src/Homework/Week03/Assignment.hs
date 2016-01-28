module Homework.Week03.Assignment (
  skips,
  localMaxima,
  histogram,
  everynth,
  isMaxima,
  buildHistogram,
  buildTuples,
  evenLists,
  evenList
) where

import Data.List (intercalate, group, maximumBy, sort, transpose)
import Data.Ord (comparing)

-- #1
skips :: [a] -> [[a]]
skips x = map (\n -> everynth n x) [1..length x]

-- Use an arithmetic sequence to build a list of indexes to pull outputs
-- e.g. when we want the every second item:
-- n=2, length s == 10 ---> [1,3..9] = [1,3,5,7,9]
-- n=3, length s == 10 ---> [2,5..9] = [2,5,8]
--
-- "s !! i" pulls out the elements by index
everynth :: Int -> [a] -> [a]
everynth n s = [s !! i | i <- [n-1,n-1+n..(length s)-1]]


-- #2
localMaxima :: [Integer] -> [Integer]
localMaxima x = map (\(s,i) -> x !! (fromIntegral i)) (filter (\(s,i) ->s) (zip (map (\n -> isMaxima n x) [1..((length x)-2)]) [1..]))

isMaxima :: Int -> [Integer] -> Bool
isMaxima n x
    | length x < 2 = False
    | x!!(n-1) < x!!(n) && x!!(n+1) < x!!(n) = True
    | otherwise = False


-- #3
histogram :: [Integer] -> String
histogram i = unlines . reverse $ map (intercalate $ "") $ transpose . evenLists . buildHistogram . buildTuples $ i

-- Split the array into pairs, (number, count)
-- We also force each number from 0..9 to be present
buildTuples :: [Integer] -> [(Integer, Int)]
buildTuples x = map (\n -> (head n, (length n)-1)) ((group.sort) (x ++ [0..9]))

-- Take a pair and build a horizontal histogram
-- e.g. (0, 4) ---> ["0", "=", "*", "*", "*", "*"]
-- e.g. (1, 2) ---> ["1", "=", "*", "*"]
buildHistogram :: [(Integer, Int)] -> [[String]]
buildHistogram l = map (\(num, count) -> show num : "=" : [] ++ (replicate count "*")) l

-- In order to use transpose, we need to make sure we have the same number of
-- elements in each list
evenLists :: [[String]] -> [[String]]
evenLists x =
    let max = length (maximumBy (comparing length) x)
    in map (\n -> evenList max n) x

evenList :: Int -> [String] -> [String]
evenList max l
    | length l < max = l ++ (replicate (max - (length l)) " ")
    | otherwise = l

