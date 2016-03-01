{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Homework.Week07.Assignment (
  ynToBool,
  parseData,
  parseMarkets,
  loadData,
  search,
  firstFound,
  lastFound,
  allFound,
  numberFound,
  orderedNtoS,
  Market(..),
  OrdList(..),
  Searcher(..)
) where

import Data.Aeson
import Data.Monoid
import GHC.Generics
import Data.List

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- #1
ynToBool :: Value -> Value
ynToBool (String "Y") = Bool True
ynToBool (String "N") = Bool False
ynToBool (Array a) = Array (fmap ynToBool a)
ynToBool (Object a) = Object (fmap ynToBool a)
ynToBool x = x

-- #2
parseData :: B.ByteString -> Either String Value
parseData x = fmap ynToBool (eitherDecode x)

-- #3
data Market = Market { marketname :: T.Text
                     , x :: Double
                     , y :: Double
                     , state :: T.Text } deriving (Eq, Show, Generic)

instance FromJSON Market

parseMarkets :: B.ByteString -> Either String [Market]
parseMarkets s = fmap ((\(Success a) -> a) . fromJSON) (parseData s)

-- #4
loadData :: IO [Market]
loadData = do
  str <- B.readFile "markets.json"
  return (either fail id (parseMarkets str))

-- #5
data OrdList a = OrdList { getOrdList :: [a] } deriving (Eq, Show)

instance Ord a => Monoid (OrdList a) where
  mempty = OrdList []
  mappend (OrdList a) (OrdList b) = OrdList (sort $ a ++ b)

-- #6
type Searcher m = T.Text -> [Market] -> m

-- Equivalent to:
-- Monoid m => (Market -> m) -> T.Text -> [Market] -> m
search :: Monoid m => (Market -> m) -> Searcher m
search f text markets = go markets
  where go [] = mempty
        go (x@(Market {marketname = name}):xs)
          | T.isInfixOf text name = (f x) <> (go xs)
          | otherwise = go xs

-- #7
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

firstFound :: Searcher (Maybe Market)
firstFound text markets = safeHead $ allFound text markets

-- #8
lastFound :: Searcher (Maybe Market)
lastFound text markets = safeHead . reverse $ allFound text markets

-- #9
allFound :: Searcher [Market]
allFound text markets = search (:[]) text markets

-- #10
numberFound :: Searcher Int
numberFound text markets = length $ allFound text markets

-- #11
orderedNtoS :: Searcher [Market]
orderedNtoS text markets = sortOn (\a -> y a) $
  allFound text markets
