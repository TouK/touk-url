-- |
-- Module: Handler.Stats
-- Copyright: (c) 2015 TouK
-- Maintainer: Przemysław Kopański pkp@touk.pl

module Handler.Stats where

import Import
import qualified Data.HashMap.Strict as M
import Prelude (foldl)

-- | Type representing counter, using Key URL to store original url info
type VisitCounter = M.HashMap Text (Key URL, Int)

-- | Type representing counter with resolved original url
type ResolvedVisitCounter = M.HashMap Text (Text, Int)

-- | Returns all visits counted, with creation date
getStatsR :: Handler Html
getStatsR = do
  logs <- (runDB $ selectList [VisitEncoded !=. ""] [Asc VisitOriginal])
  visits <- (fmap M.toList) . resolveVisitCounter . countVisits . map entityVal $ logs
  defaultLayout $(widgetFile "get-stats")

-- | Constructs a map with counted visits
countVisits :: [Visit] -> VisitCounter
countVisits =
  foldl (\hmap (Visit enc org _ _) -> go enc org hmap) M.empty
  where
    go :: Text -> Key URL -> VisitCounter -> VisitCounter
    go enc org = M.insertWith (\_ (key, allVisits) -> (key,allVisits+1)) enc (org, 1)

-- | Resolves an original url given key
resolveOriginalURL :: Key URL -> Handler Text
resolveOriginalURL key = do
  (Entity _ (URL org _ _ _)):_ <- runDB $ selectList [URLId ==. key] []
  return org

-- | Resolves one 'VisitCounter' map value
resolveTuple :: (Key URL, Int) -> Handler (Text, Int)
resolveTuple (key, allVisits) = do
  url <- resolveOriginalURL key
  return (url, allVisits)

-- | Resolves all elements of 'VisitCounter'
resolveVisitCounter :: VisitCounter -> Handler ResolvedVisitCounter
resolveVisitCounter = traverse resolveTuple
