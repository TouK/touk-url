-- |
-- Module: Handler.StatsApi
-- Copyright: (c) 2015 TouK
-- Maintainer: Przemysław Kopański pkp@touk.pl

module Handler.StatsApi where

import Import
import Database.Persist.Sql (rawSql)


-- | Handles all API calls
getStatsApiR :: Text -- ^ Response kind parameter
             -> Handler Value
getStatsApiR "visits" = printVisits
getStatsApiR "urls" = printUrls
getStatsApiR _ = return $ object [ "error" .= ("invalid option" :: Text) ]

-- | Returns all visits from database in JSON
printVisits :: Handler Value
printVisits = do
  logs <- runDB $ rawSql
            "SELECT ??, ?? \
            FROM visit INNER JOIN url \
            ON visit.original=url.id" [] :: Handler [(Entity Visit, Entity URL)]
  return $ array . map Log $ logs

-- | Returns all urls created from database in JSON
printUrls :: Handler Value
printUrls = do
  urls <- runDB $ selectList [] [] :: Handler [Entity URL]

  return $ array . map entityVal $ urls

-- | Wrapper for raw visits returned from database
data Log = Log (Entity Visit, Entity URL)

instance ToJSON Log where
    toJSON (Log ((Entity _ (Visit enc _ ip date)), (Entity _ URL{uRLOriginal=url}))) =
      object [ "date"        .= (show date)
              ,"originalUrl" .= url
              ,"encoded"     .= enc
              ,"ip"          .= ip ]
