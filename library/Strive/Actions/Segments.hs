{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/v3/segments/>
module Strive.Actions.Segments
    ( exploreSegments
    , getSegment
    , getSegmentEfforts
    , getSegmentLeaderboard
    , getStarredSegments
    ) where

import Data.Aeson (encode, (.:))
import Data.Aeson.Types (parseEither)
import Data.ByteString.Char8 (pack, singleton)
import Data.ByteString.Lazy (toStrict)
import Data.List (intercalate)
import Data.Monoid ((<>))
import Data.Time.Clock (UTCTime)
import Strive.Client (Client)
import Strive.Client.HTTP (get)
import Strive.Objects (EffortDetailed, SegmentDetailed, SegmentExplorerEntry,
                       SegmentLeaderboard, SegmentLeaderboardEntry,
                       SegmentSummary)
import Strive.Types (AthleteId, Page, PerPage, SegmentId)
import Strive.Utilities (paginate, queryToSimpleQuery)

-- | <http://strava.github.io/api/v3/segments/#explore>
exploreSegments :: Client -> (Double, Double, Double, Double) -> Maybe String -> Maybe Integer -> Maybe Integer -> IO (Either String [SegmentLeaderboard])
exploreSegments client (south, west, north, east) activityType minCat maxCat = get client resource query
  where
    resource = "segments/explore"
    query = queryToSimpleQuery
        [ ("bounds", Just (pack bounds))
        , ("activity_type", fmap pack activityType)
        , ("min_cat", fmap (pack . show) minCat)
        , ("max_cat", fmap (pack . show) maxCat)
        ]
    bounds = intercalate "," (fmap show [south, west, north, east])

-- | <http://strava.github.io/api/v3/segments/#retrieve>
getSegment :: Client -> SegmentId -> IO (Either String SegmentDetailed)
getSegment client segmentId = get client resource query
  where
    resource = "segments/" <> show segmentId
    query = [] :: [(String, String)]

-- | <http://strava.github.io/api/v3/segments/#efforts>
getSegmentEfforts :: Client -> SegmentId -> Maybe AthleteId -> Maybe (UTCTime, UTCTime) -> Page -> PerPage -> IO (Either String [EffortDetailed])
getSegmentEfforts client segmentId athleteId range page perPage = get client resource query
  where
    resource = "segments/" <> show segmentId <> "/all_efforts"
    query = paginate page perPage <> queryToSimpleQuery
        [ ("athlete_id", fmap (pack . show) athleteId)
        , ("start_date_local", fmap (toStrict . encode . fst) range)
        , ("end_date_local", fmap (toStrict . encode . snd) range)
        ]

-- | <http://strava.github.io/api/v3/segments/#leaderboard>
getSegmentLeaderboard :: Client -> SegmentId -> Maybe Char -> Maybe String -> Maybe String -> Maybe Bool -> Maybe Integer -> Maybe String -> Page -> PerPage -> IO (Either String [SegmentLeaderboardEntry])
getSegmentLeaderboard client segmentId gender ageGroup weightClass following clubId dateRange page perPage = do
    object <- get client resource query
    let leaders = either Left (parseEither (.: "entries")) object
    return leaders
  where
    resource = "segments/" <> show segmentId <> "/leaderboard"
    query = paginate page perPage <> queryToSimpleQuery
        [ ("gender", fmap singleton gender)
        , ("age_group", fmap pack ageGroup)
        , ("weight_class", fmap pack weightClass)
        , ("following", fmap (toStrict . encode) following)
        , ("club_id", fmap (pack . show) clubId)
        , ("date_range", fmap pack dateRange)
        ]

-- | <http://strava.github.io/api/v3/segments/#starred>
getStarredSegments :: Client -> Page -> PerPage -> IO (Either String [SegmentSummary])
getStarredSegments client page perPage = get client resource query
  where
    resource = "segments/starred"
    query = paginate page perPage
