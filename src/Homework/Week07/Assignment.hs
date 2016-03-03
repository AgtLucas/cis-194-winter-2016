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
import Data.List (sort, sortOn)
import Data.Maybe (listToMaybe)
import Data.Monoid
import GHC.Generics

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- #1
ynToBool :: Value -> Value
ynToBool (String "Y") = Bool True
ynToBool (String "N") = Bool False
ynToBool (Array xs)   = Array (fmap ynToBool xs)
ynToBool (Object xs)  = Object (fmap ynToBool xs)
ynToBool other        = other

-- #2
parseData :: B.ByteString -> Either String Value
parseData = fmap ynToBool . eitherDecode

-- #3
data Market = Market {
    marketname :: T.Text,
    x :: Double,
    y :: Double,
    state :: T.Text
} deriving (Eq, Show, Generic)

instance FromJSON Market

parseMarkets :: B.ByteString -> Either String [Market]
parseMarkets = eitherDecode

-- #4
loadData :: IO [Market]
loadData = do
    jsonMarkets <- B.readFile "markets.json"
    let markets = parseMarkets jsonMarkets
    either fail return markets

-- #5
data OrdList a = OrdList {
    getOrdList :: [a]
} deriving (Eq, Show)

instance Ord a => Monoid (OrdList a) where
    mempty = OrdList []
    mappend (OrdList xs) (OrdList ys) = OrdList (sort $ xs <> ys)

-- #6
type Searcher m = T.Text -> [Market] -> m

search :: Monoid m => (Market -> m) -> Searcher m
search toMonoid name = mconcat . fmap toMonoid . filter (byName name)
    where byName name = \ market -> name `T.isInfixOf` (marketname market)

-- #7
firstFound :: Searcher (Maybe Market)
firstFound = compose2 headMaybe allFound

-- #8
lastFound :: Searcher (Maybe Market)
lastFound = compose2 (headMaybe . reverse) allFound

-- #9
allFound :: Searcher [Market]
allFound = compose2 id search ( : [])

-- #10
numberFound :: Searcher Int
numberFound = compose2 length allFound

-- #11
orderedNtoS :: Searcher [Market]
orderedNtoS = compose2 (sortOn y) allFound

-- helpers
headMaybe = listToMaybe

compose2 :: (c -> d) -> (a -> b -> c) -> a -> b -> d
-- compose2 = (.) . (.)
compose2 g f x y = g $ f x y
