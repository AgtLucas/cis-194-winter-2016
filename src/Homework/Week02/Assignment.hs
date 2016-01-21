module Homework.Week02.Assignment (
  build,
  inOrder,
  insert,
  parse,
  parseMessage,
  whatWentWrong,
  LogMessage(..),
  MessageTree(..),
  MessageType(..),
  TimeStamp
) where

import Homework.Week02.Log

-- #1a
parseMessage :: String -> LogMessage
parseMessage = parseMessageWords . words

parseMessageWords :: [String] -> LogMessage
parseMessageWords ("E":severity:timestamp:xs) = LogMessage (Error (read severity)) (read timestamp) (unwords xs)
parseMessageWords ("I":timestamp:xs) = LogMessage Info (read timestamp) (unwords xs)
parseMessageWords ("W":timestamp:xs) = LogMessage Warning (read timestamp) (unwords xs)
parseMessageWords a = Unknown $ unwords a
--


-- #1b
parse :: String -> [LogMessage]
parse line = map parseMessage (lines line)

-- #2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown str) tree = tree -- unknown just returns the tree
insert m (Leaf) = Node Leaf m Leaf
insert m@(LogMessage _ timestamp _) (Node l root@(LogMessage _ rootTimestamp _) r)
    | timestamp < rootTimestamp = (Node (insert m l) root r)
    | otherwise = (Node l root (insert m r))

-- #3
build :: [LogMessage] -> MessageTree
build l = foldl (\acc m -> insert m acc) Leaf l

-- #4
inOrder :: MessageTree -> [LogMessage]
inOrder = undefined

-- #5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = undefined
