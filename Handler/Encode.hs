module Handler.Encode where

import Import
import RandStr as R
import Data.Char
import Text.Parsec as P hiding ((<|>))
import Text.Parsec.Text

getEncodeR :: Handler Html
getEncodeR = defaultLayout $(widgetFile "get-encode")

postEncodeR :: Handler Html
postEncodeR = do
  urlReceived <- runInputPost $ ireq textField "url"
  case isValidUrl urlReceived of
    True -> do
      encodedUrl <- getShortUrl urlReceived
      defaultLayout $(widgetFile "post-encode")
    False -> redirect EncodeR

getShortUrl :: Text -> Handler Text
getShortUrl url = do
  -- look for entries with given full url
  dbProbe <- runDB $ selectList [URLOriginal ==. url] []
  -- if found, return the existing short url
  -- if not, create the new one
  case dbProbe of
    [] -> generateNewShort url
    Entity _ (URL _ short):_ -> return short
    --_ -> error "should never happen"

generateNewShort :: Text -> Handler Text
generateNewShort url = do
  -- generate random string
  candidate <- lift $ R.randomStringWithLen 6
  dbProbe <- runDB $ selectList [URLEncoded ==. candidate] []
  -- if it is already taken, generate new
  -- if not, insert entry and return it
  case dbProbe of
    [] -> fmap (const candidate) (runDB $ insert $ URL url candidate)
    _ -> generateNewShort url

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
  optional $ char ':'
  optional $ many1 digit
  optional $ char '/'
  optional $ sepEndBy1 word (char '/')
  optional $ oneOf "?#"
  -- TODO: parse queries
  optional $ many anyChar
  eof
