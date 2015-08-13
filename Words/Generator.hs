{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Words.Generator where

import ClassyPrelude
import Data.Text (Text)
import Data.Vector (Vector)
import System.Random
import Control.Monad
import System.IO
import Data.Aeson
import GHC.Generics

import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B

data DataBase = DataBase
  { adjectives :: Vector Text
  , nouns :: Vector Text
  , verbs :: Vector Text
  } deriving (Show, Generic)

instance FromJSON DataBase
instance ToJSON DataBase

getRandomElement :: Vector Text -> IO Text
getRandomElement vect = do
  randIndex <- randomRIO (0, length vect - 1)
  case vect V.!? randIndex of
    Just num -> return num
    Nothing -> getRandomElement vect

getRandomPhrase :: DataBase -> IO Text
getRandomPhrase (DataBase adjs nouns verbs) = do
  adj <- getRandomElement adjs
  noun1 <- getRandomElement nouns
  verb <- getRandomElement verbs
  noun2 <- getRandomElement nouns
  return $ T.intercalate "-" [adj, noun1, verb, noun2]


loadDataBase :: IO (Maybe DataBase)
loadDataBase = fmap decode $ B.readFile "db.json"

getPhrase :: IO (Maybe Text)
getPhrase = do
  dbMaybe <- loadDataBase
  case dbMaybe of
    Just db -> do
      phrase <- getRandomPhrase db
      return $ Just phrase
    Nothing -> return Nothing
