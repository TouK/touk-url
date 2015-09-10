module Handler.Stats where

import Import
import qualified Data.HashMap.Strict as M
import Prelude (foldl)

type VisitCounter = M.HashMap Text (Key URL, Int)
type ResolvedVisitCounter = M.HashMap Text (Text, Int)

-- | Returns all visits counted, with creation date
getStatsR :: Handler Html
getStatsR = do
  logs <- (runDB $ selectList [VisitEncoded !=. ""] [Asc VisitOriginal])
  visits <- (fmap M.toList) . resolveVisitCounter . countVisits . map entityToVisit $ logs
  defaultLayout $(widgetFile "get-stats")


entityToVisit :: Entity Visit -> Visit
entityToVisit (Entity _ visit) = visit

countVisits :: [Visit] -> VisitCounter
countVisits =
  foldl (\hmap (Visit enc org _ _) -> go hmap enc org) M.empty
  where
    go :: VisitCounter -> Text -> Key URL -> VisitCounter
    go hashmap enc org = M.insertWith (\_ (key, allVisits) -> (key,allVisits+1)) enc (org, 1) hashmap

resolveOriginalURL :: Key URL -> Handler Text
resolveOriginalURL key = do
  (Entity _ (URL org _ _ _)):_ <- runDB $ selectList [URLId ==. key] []
  return org

resolveTuple :: (Key URL, Int) -> Handler (Text, Int)
resolveTuple (key, allVisits) = do
  url <- resolveOriginalURL key
  return (url, allVisits)

resolveVisitCounter :: VisitCounter -> Handler ResolvedVisitCounter
resolveVisitCounter = traverse resolveTuple
