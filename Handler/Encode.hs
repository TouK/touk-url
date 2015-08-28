{-# LANGUAGE DeriveGeneric #-}
module Handler.Encode where

import Import
import RandStr as R
import Data.Char
import Text.Parsec as P hiding ((<|>))
import Text.Parsec.Text
import qualified Data.Aeson as J
import Words.Generator

errorMsg :: Text
errorMsg = "Invalid URL"

data Url = Url Text
  deriving (Show, Generic)

instance FromJSON Url where
  parseJSON (Object v) = Url <$>
                        v .: "url"
  parseJSON _          = mzero

getEncodeR :: Handler Html
getEncodeR = defaultLayout $(widgetFile "get-encode")

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

getShortUrl :: Text -> Handler (Text, Text)
getShortUrl url = do
  -- look for entries with given full url
  dbProbe <- runDB $ selectList [URLOriginal ==. url] []
  -- if found, return the existing short url
  -- if not, create the new one
  case dbProbe of
    [] -> generateNewShort url
    Entity _ URL{uRLEncoded=short, uRLFunEncoded=fun}:_ -> return (short,fun)
    --_ -> error "should never happen"

generateNewShort :: Text -> Handler (Text,Text)
generateNewShort url = do
  wordsdb <- appWordsDatabase <$> getYesod
  -- generate random string
  -- dbProbe <- runDB $ selectList [URLEncoded ==. candidate] []
  -- case dbProbe of
    -- [] -> fmap (const candidate) (runDB $ insert $ URL url candidate)
    -- _ -> generateNewShort url
  randString <- getUniquePhrase (R.randomStringWithLen 6) URLEncoded
  randFunString <- getUniquePhrase (getRandomPhrase wordsdb) URLFunEncoded
  _ <- runDB $ insert $ URL url randString randFunString
  return (randString, randFunString)


getUniquePhrase :: IO Text -> EntityField URL Text -> Handler Text
getUniquePhrase gen field = do
  candidate <- lift gen
  condition <- isUniquePhrase field candidate
  if condition then
      return candidate
    else
      getUniquePhrase gen field

isUniquePhrase :: EntityField URL Text -> Text -> Handler Bool
isUniquePhrase field candidate = do
  dbProbe <- runDB $ selectList [field ==. candidate] []
  case dbProbe of
    [] -> return True
    _ -> return False


isValidUrl :: Text -> Bool
isValidUrl url =
  case parse parser "failmsg" url of
    Left _ -> False
    _ -> True


word :: Parser Text
word = fmap pack $ many1 $ satisfy ((\a b c -> a || b || c) <$> isAlpha <*> isDigit <*> (=='-'))

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
