module Handler.Encode where

import Import
import qualified Data.Text as T
import RandStr as R

getEncodeR :: Handler Html
getEncodeR = defaultLayout $(widgetFile "get-encode")

postEncodeR :: Handler Html
postEncodeR = do
  urlReceived <- runInputPost $ ireq textField "url"

  encodedUrl <- getShortUrl urlReceived
  defaultLayout $(widgetFile "post-encode")

getShortUrl :: Text -> Handler Text
getShortUrl url = do
  -- look for entries with given full url
  dbProbe <- runDB $ selectList [URLOriginal ==. url] []
  -- if found, return the existing short url
  -- if not, create the new one
  case dbProbe of
    [] -> generateNewShort url
    (Entity _ (URL _ short)):_ -> return short
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
