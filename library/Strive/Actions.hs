-- | Functions for performing actions against the API.
module Strive.Actions where

import Data.Aeson (Value, encode)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy (toStrict)
import Data.Default (Default, def)
import Data.Monoid ((<>))
import Data.Time.Clock (UTCTime)
import Network.HTTP.Types (Query, renderQuery, toQuery)
import Strive.Client (Client, buildClient)
import Strive.Internal.HTTP (delete, get, post, put)
import qualified Strive.Options as O
import qualified Strive.Types as T

-- | Helper function for easily performing actions.
with :: Default a => [a -> a] -> a
with = foldr ($) def

-- * Authentication

-- | <http://strava.github.io/api/v3/oauth/#get-authorize>
buildAuthorizeUrl :: Integer -> String -> O.BuildAuthorizeUrlOptions -> String
buildAuthorizeUrl clientId redirectUrl options =
  "https://www.strava.com/oauth/authorize" <> unpack (renderQuery True query)
 where
  query = toQuery
    [ ("client_id", show clientId)
    , ("redirect_url", redirectUrl)
    , ("response_type", "code")
    ] <> toQuery options

-- | <http://strava.github.io/api/v3/oauth/#post-token>
exchangeToken :: Integer -> String -> String -> IO (Either String T.TokenExchangeResponse)
exchangeToken clientId clientSecret code = do
  client <- buildClient "" -- TODO: This is kind of dumb.
  post client resource query
 where
  resource = "oauth/token"
  query =
    [ ("client_id", show clientId)
    , ("client_secret", clientSecret)
    , ("code", code)
    ]

-- | <http://strava.github.io/api/v3/oauth/#deauthorize>
deauthorize :: Client -> IO (Either String T.DeauthorizationResponse)
deauthorize client = post client resource query
 where
  resource = "oauth/deauthorize"
  query = [] :: Query

-- * Athletes

-- | <http://strava.github.io/api/v3/athlete/#get-details>
getCurrentAthlete :: Client -> IO (Either String T.AthleteDetailed)
getCurrentAthlete client = get client resource query
 where
  resource = "api/v3/athlete"
  query = [] :: Query

-- | <http://strava.github.io/api/v3/athlete/#get-another-details>
getAthlete :: Client -> Integer -> IO (Either String T.AthleteSummary)
getAthlete client athleteId = get client resource query
 where
  resource = "api/v3/athletes/" <> show athleteId
  query = [] :: Query

-- | <http://strava.github.io/api/v3/athlete/#update>
updateCurrentAthlete :: Client -> O.UpdateCurrentAthleteOptions -> IO (Either String T.AthleteDetailed)
updateCurrentAthlete client options = put client resource query
 where
  resource = "api/v3/athlete"
  query = toQuery options

-- | <http://strava.github.io/api/v3/athlete/#koms>
getAthleteCrs :: Client -> Integer -> O.GetAthleteCrsOptions -> IO (Either String [T.EffortDetailed])
getAthleteCrs client athleteId options = get client resource query
 where
  resource = "api/v3/athletes/" <> show athleteId <> "/koms"
  query = toQuery options

-- * Friends and Followers

-- | <http://strava.github.io/api/v3/follow/#friends>
getCurrentFriends :: Client -> O.GetCurrentFriendsOptions -> IO (Either String [T.AthleteSummary])
getCurrentFriends client options = get client resource query
 where
  resource = "api/v3/athlete/friends"
  query = toQuery options

-- | <http://strava.github.io/api/v3/follow/#friends>
getFriends :: Client -> Integer -> O.GetFriendsOptions -> IO (Either String [T.AthleteSummary])
getFriends client athleteId options = get client resource query
 where
  resource = "api/v3/athletes/" <> show athleteId <> "/friends"
  query = toQuery options

-- | <http://strava.github.io/api/v3/follow/#followers>
getCurrentFollowers :: Client -> O.GetCurrentFollowersOptions -> IO (Either String [T.AthleteSummary])
getCurrentFollowers client options = get client resource query
 where
  resource = "api/v3/athlete/followers"
  query = toQuery options

-- | <http://strava.github.io/api/v3/follow/#followers>
getFollowers :: Client -> Integer -> O.GetFollowersOptions -> IO (Either String [T.AthleteSummary])
getFollowers client athleteId options = get client resource query
 where
  resource = "api/v3/athletes/" <> show athleteId <> "/followers"
  query = toQuery options

-- | <http://strava.github.io/api/v3/follow/#both>
getCommonFriends :: Client -> Integer -> O.GetCommonFriendsOptions -> IO (Either String [T.AthleteSummary])
getCommonFriends client athleteId options = get client resource query
 where
  resource = "api/v3/athletes/" <> show athleteId <> "/both-following"
  query = toQuery options

-- * Activities

-- | <http://strava.github.io/api/v3/activities/#create>
createActivity :: Client -> String -> String -> UTCTime -> Integer -> O.CreateActivityOptions -> IO (Either String T.ActivityDetailed)
createActivity client name type_ startDateLocal elapsedTime options = post client resource query
 where
  resource = "api/v3/activities"
  query = toQuery
    [ ("name", name)
    , ("type", type_)
    , ("start_date_local", unpack (toStrict (encode startDateLocal)))
    , ("elapsed_time", show elapsedTime)
    ] <> toQuery options

-- | <http://strava.github.io/api/v3/activities/#get-details>
getActivity :: Client -> Integer -> O.GetActivityOptions -> IO (Either String T.ActivitySummary)
getActivity client activityId options = get client resource query
 where
  resource = "api/v3/activities/" <> show activityId
  query = toQuery options

-- | <http://strava.github.io/api/v3/activities/#put-updates>
updateActivity :: Client -> Integer -> O.UpdateActivityOptions -> IO (Either String T.ActivityDetailed)
updateActivity client activityId options = put client resource query
 where
  resource = "api/v3/activities/" <> show activityId
  query = toQuery options

-- | <http://strava.github.io/api/v3/activities/#delete>
deleteActivity :: Client -> Integer -> IO (Either String Value)
deleteActivity client activityId = delete client resource query
 where
  resource = "api/v3/activities/" <> show activityId
  query = [] :: Query
