{-# LANGUAGE OverloadedStrings #-}
module Handler.Shorten where

import Import
import Data.Text as T
import Types

postShortenR :: Handler Value
postShortenR = do
    url <- requireJsonBody :: Handler URL
    return $ object [ "url" .= ("test" :: Text) ]
