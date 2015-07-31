module Handler.Encode where

import Import
import qualified Data.Text as T
import RandStr as R

getEncodeR :: Handler Html
getEncodeR = defaultLayout $(widgetFile "get-encode")

postEncodeR :: Handler Html
postEncodeR = do
  urlReceived <- runInputPost $ ireq textField "url"
  encodedUrl <- lift $ R.randomStringWithLen 6
  defaultLayout $(widgetFile "post-encode")
