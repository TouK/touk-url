{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Types where

import Data.Aeson
import Data.Text
import GHC.Generics

data URL =
    URL {
     urlWrapped :: !Text
    } deriving (Generic)

instance FromJSON URL
instance ToJSON URL
