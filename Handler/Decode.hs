{-# LANGUAGE LambdaCase, OverloadedStrings #-}
-- |
-- Module: Handler.Decode
-- Copyright: (c) 2015 TouK
-- Maintainer: Przemysław Kopański pkp@touk.pl

module Handler.Decode where

import Import
import Network.Wai

-- | Redirects to saved url, if it's not found then redirects to main page
getDecodeR :: Text -> Handler Html
getDecodeR encoded = do
    realUrl <- getRedirectionUrl encoded
    redirect realUrl

getRedirectionUrl :: Text -> Handler Text
getRedirectionUrl encoded = do
  maybeUrl <- findShortUrl encoded
  ipAddress <- fmap (maybe "unknown" id) getIpAddress
  case maybeUrl of
    Just (key, original) -> do
      addOneVisit key encoded ipAddress
      return original
    Nothing ->
      return "http://www.touk.pl"

-- | Gets ip address of client if there's one
-- currently doesn't return Nothing, saved for future usage
getIpAddress :: Handler (Maybe Text)
getIpAddress = do
  request <- waiRequest
  return . Just . pack . show $ remoteHost request

-- | Saves in database information about short url visit
addOneVisit :: Key URL -- ^ Original url key in database
            -> Text -- ^ Encoded url
            -> Text -- ^ IP address
            -> Handler ()
addOneVisit original encoded ip = do
  time <- lift getCurrentTime
  _ <- runDB $ insert $ Visit encoded original ip time
  return ()

-- | Look for given url in database
findShortUrl :: Text -> Handler (Maybe (Key URL, Text))
findShortUrl short = do
  -- look for enties with given encoded url
  dbProbe <- runDB $ selectList ([URLEncoded ==. short] ||. [URLFunEncoded ==. short]) []
  -- if found, return original url, if not return Nothing
  case dbProbe of
    [Entity key (URL original _ _ _)] -> return $ Just (key, original)
    _ -> return Nothing
