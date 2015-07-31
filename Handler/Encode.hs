module Handler.Encode where

import Import
import qualified Data.Text as T

getEncodeR :: Handler Html
getEncodeR = defaultLayout $(widgetFile "get-encode")

postEncodeR :: Handler Html
postEncodeR = do
  urlReceived <- runInputPost $ ireq textField "url"
  encodedUrl <- return urlReceived --TODO
  defaultLayout $(widgetFile "post-encode")
