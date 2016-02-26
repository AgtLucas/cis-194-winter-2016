module Homework.Week06.Assignment (
  fib,
  fibs1,
  fibs2,
  streamToList,
  streamRepeat,
  streamMap,
  streamFromSeed,
  nats,
  ruler,
  Stream(..),
  interleaveStreams,
  biggestEvenPowerOf2
) where

-- #1a
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = (fib (n-1)) + (fib (n-2))

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- #2
fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

-- #3
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x stream) = x : streamToList stream

instance Show a => Show (Stream a) where
  show s = show $ take 20 $ streamToList s

-- #4
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x stream) = Cons (f x) (streamMap f stream)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = Cons seed (streamFromSeed f (f seed))

-- #5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = streamMap (\n -> until (biggestEvenPowerOf2 n) (subtract 1) n)
  (interleaveStreams (streamRepeat 0) (streamFromSeed (+2) 2))
-- Using interleaveStreams is faster than the naive (streamFromSeed (+1) 1)
-- It sets aall odd items to 0, which biggestEvenPowerOf2 short circuits on!

biggestEvenPowerOf2 :: Integer -> Integer -> Bool
biggestEvenPowerOf2 n x
  | x <= 0 = True
  | otherwise = (n `mod` 2^x) == 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x s1) (Cons y s2) = Cons x (Cons y (interleaveStreams s1 s2))
