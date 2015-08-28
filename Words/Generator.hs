{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: Words.Generator
-- Copyright: (c) 2015 TouK
-- Maintainer: Przemysław Kopański pkp@touk.pl

module Words.Generator where

import ClassyPrelude
import System.Random
import Data.Aeson

import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B

-- | Database of words used to produce 'funny' link
data DataBase = DataBase
  { adjectives :: Vector Text
  , nouns :: Vector Text
  , verbs :: Vector Text
  } deriving (Show, Generic)

instance FromJSON DataBase
instance ToJSON DataBase

-- | Loads words' database from file db.json
loadDataBase :: IO (Maybe DataBase)
loadDataBase = fmap decode $ B.readFile "db.json"

-- | Returns random element of a vector
getRandomElement :: Vector Text -> IO Text
getRandomElement vect = do
  randIndex <- randomRIO (0, length vect - 1)
  case vect V.!? randIndex of
    Just num -> return num
    Nothing -> getRandomElement vect

-- | Generates random phrase in pattern: adjective-noun-verb-noun
getRandomPhrase :: DataBase -> IO Text
getRandomPhrase (DataBase adjs nouns verbs) = do
  adj <- getRandomElement adjs
  noun1 <- getRandomElement nouns
  verb <- getRandomElement verbs
  noun2 <- getRandomElement nouns
  return $ T.intercalate "-" [adj, noun1, verb, noun2]

getPhrase :: IO (Maybe Text)
getPhrase = do
  dbMaybe <- loadDataBase
  case dbMaybe of
    Just db -> do
      phrase <- getRandomPhrase db
      return $ Just phrase
    Nothing -> return Nothing
