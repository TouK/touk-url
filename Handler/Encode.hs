{-# LANGUAGE DeriveGeneric #-}
-- |
-- Module: Handler.Encode
-- Copyright: (c) 2015 TouK
-- Maintainer: Przemysław Kopański pkp@touk.pl

module Handler.Encode where

import Import
import RandStr as R
import Data.Char
import Text.Parsec as P hiding ((<|>))
import Text.Parsec.Text
import qualified Data.Aeson as J
import Words.Generator

{-
  Handlers for encoding
-}


-- | Error message while URL is invalid
errorMsg :: Text
errorMsg = "Invalid URL"

data Url = Url Text
  deriving (Show, Generic)

instance FromJSON Url where
  parseJSON (Object v) = Url <$>
                        v .: "url"
  parseJSON _          = mzero

-- | Handler for GET query, returns web page
getEncodeR :: Handler Html
getEncodeR = defaultLayout $(widgetFile "get-encode")

-- | Handler for POST query, returns JSON
postEncodeR :: Handler Value
postEncodeR = do
  urlReceived <- parseJsonBody :: Handler (J.Result Url)
  render <- getUrlRender
  case urlReceived  of
    J.Error e -> return $ object ["jsonError" .= e ]
    J.Success (Url url) -> do
      if isValidUrl url then
        do
          (encoded, funEncoded) <- getShortUrl url
          return $ object ["classyEncoded" .= render (DecodeR encoded),
                           "funEncoded" .= render (DecodeR funEncoded)]
        else
          return $ object ["error" .= errorMsg]

-- | Checks whether shortened URLs already exist in database, if not, generates them
getShortUrl :: Text -> Handler (Text, Text)
getShortUrl url = do
  -- look for entries with given full url
  dbProbe <- runDB $ selectList [URLOriginal ==. url] []
  -- if found, return the existing short url
  -- if not, create the new one
  case dbProbe of
    [] -> generateNewShort url
    Entity _ URL{uRLEncoded=short, uRLFunEncoded=fun}:_ -> return (short,fun)

-- | Generates unique shortened URLs for the given one
generateNewShort :: Text -> Handler (Text,Text)
generateNewShort url = do
  wordsdb <- appWordsDatabase <$> getYesod

  -- generate random string
  randString <- getUniquePhrase (R.randomStringWithLen 6) URLEncoded
  randFunString <- getUniquePhrase (getRandomPhrase wordsdb) URLFunEncoded

  currentTime <- lift getCurrentTime
  _ <- runDB $ insert $ URL url randString randFunString currentTime
  return (randString, randFunString)

-- | Returns unique phrase not existing in database
getUniquePhrase :: IO Text -- ^Monadic phrase generator
                -> EntityField URL Text -- ^Criteria of given phrase
                -> Handler Text -- ^Unique phrase which is not already used in database
getUniquePhrase gen field = do
  candidate <- lift gen
  condition <- isUniquePhrase field candidate
  if condition then
      return candidate
    else
      getUniquePhrase gen field

-- | Looks for given criteria in database and checks if given value would be unique
isUniquePhrase :: EntityField URL Text -- ^Criteria in database
               -> Text -- ^Value of which uniqueness is checked
               -> Handler Bool
isUniquePhrase field candidate = do
  dbProbe <- runDB $ selectList [field ==. candidate] []
  case dbProbe of
    [] -> return True
    _ -> return False

-- | Checks wether given URL is valid
isValidUrl :: Text -> Bool
isValidUrl url =
  case parse parser "failmsg" url of
    Left _ -> False
    _ -> True

-- | Definition of word used in URL standards (allowed alphabet with digits and "-")
word :: Parser Text
word = fmap pack $ many1 $ satisfy ((\a b c -> a || b || c) <$> isAlpha <*> isDigit <*> (=='-'))

-- | URL parser, used to check wether given URL is valid
parser :: Parser ()
parser = do
  _ <- P.try (string "https") <|> string "http"
  _ <- string "://"
  _ <- sepBy1 word (char '.')
  optional $ char ':' >> many1 digit
  optional $ char '/'
  optional $ sepEndBy1 word (char '/')
  optional $ oneOf "?#"
  -- TODO: parse queries
  optional $ many anyChar
  eof
