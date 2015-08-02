{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Handler.Decode where

import Import

getDecodeR :: Text -> Handler Html
getDecodeR text = do
    render <- getUrlRender
    realUrl <- fmap (\case {(Just url) -> url;
                             Nothing -> render EncodeR})
              (findShortUrl text)
    --defaultLayout $(widgetFile "decode")
    redirect realUrl

findShortUrl :: Text -> Handler (Maybe Text)
findShortUrl short = do
  -- look for enties with given encoded url
  dbProbe <- runDB $ selectList [URLEncoded ==. short] []
  -- if found, return original url, if not return Nothing
  case dbProbe of
    [Entity _ (URL original _)] -> return $ Just original
    _ -> return Nothing
