{-# LANGUAGE LambdaCase, OverloadedStrings #-}
-- |
-- Module: Handler.Decode
-- Copyright: (c) 2015 TouK
-- Maintainer: Przemysław Kopański pkp@touk.pl

module Handler.Decode where

import Import

-- | Redirects to saved url, if it's not found then redirects to main page
getDecodeR :: Text -> Handler Html
getDecodeR text = do
    render <- getUrlRender
    realUrl <- fmap (\case {(Just url) -> url;
                             Nothing -> render EncodeR})
              (findShortUrl text)
    --defaultLayout $(widgetFile "decode")
    redirect realUrl

-- | Look for given url in database
findShortUrl :: Text -> Handler (Maybe Text)
findShortUrl short = do
  -- look for enties with given encoded url
  dbProbe <- runDB $ selectList ([URLEncoded ==. short] ||. [URLFunEncoded ==. short]) []
  -- if found, return original url, if not return Nothing
  case dbProbe of
    [Entity _ (URL original _ _)] -> return $ Just original
    _ -> return Nothing
