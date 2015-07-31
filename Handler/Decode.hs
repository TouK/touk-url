module Handler.Decode where

import Import

getDecodeR :: Text -> Handler Html
getDecodeR text = do
    realUrl <- return $ text
    defaultLayout $(widgetFile "decode")
