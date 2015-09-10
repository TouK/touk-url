module Handler.StatsApi where

import Import
import Database.Persist.Sql (rawSql)
import Data.Aeson (encode)

data Log = Log (Entity Visit, Entity URL)


-- | Returns all visits in JSON
getStatsApiR :: Text -- ^ Response type parameter
             -> Handler Value
getStatsApiR "visits" = do
  logs <- runDB $ rawSql
            "SELECT ??, ?? \
            FROM visit INNER JOIN url \
            ON visit.original=url.id" []

  return $ encode (logs :: [Log])

getStatsApiR _ = return $ object [ "error" .= ("invalid option" :: Text) ]


instance ToJSON Log where
    toJSON (Log ((Entity _ (Visit enc _ ip date)), (Entity _ URL{uRLOriginal=url}))) =
      object [ "date"        .= (show date)
              ,"originalUrl" .= url
              ,"encoded"     .= enc
              ,"ip"          .= ip ]
