-- | Functions for performing actions against the API.
module Strive.Actions where

import Data.Aeson (FromJSON, Value, encode)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy (toStrict)
import Data.Default (Default, def)
import Data.List (intercalate)
import Data.Monoid ((<>))
import Data.Time.Clock (UTCTime)
import Network.HTTP.Conduit (RequestBody (RequestBodyBS), requestBody,
                             responseBody, responseStatus)
import Network.HTTP.Types (Query, methodDelete, methodPost, noContent204,
                           renderQuery, toQuery)
import Strive.Client (Client, buildClient)
import Strive.Internal.HTTP (buildRequest, decodeValue, delete, get,
                             performRequest, post, put)
import qualified Strive.Options as O
import qualified Strive.Types as T

-- | Helper function for easily performing actions.
with :: Default a => [a -> a] -> a
with = foldr ($) def

-- | Infix alias of 'with'.
infixr 0 ?
(?) :: Default a => (a -> b) -> [a -> a] -> b
(?) = (. with)

-- * Authentication

-- | <http://strava.github.io/api/v3/oauth/#get-authorize>
buildAuthorizeUrl :: Integer -> String -> O.BuildAuthorizeUrlOptions -> String
buildAuthorizeUrl clientId redirectUri options =
  "https://www.strava.com/oauth/authorize" <> unpack (renderQuery True query)
 where
  query = toQuery
    [ ("client_id", show clientId)
    , ("redirect_uri", redirectUri)
    , ("response_type", "code")
    ] <> toQuery options

-- | <http://strava.github.io/api/v3/oauth/#post-token>
exchangeToken :: Integer -> String -> String -> IO (Either String T.TokenExchangeResponse)
exchangeToken clientId clientSecret code = do
  client <- buildClient ""
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
deleteActivity :: Client -> Integer -> IO (Either String ())
deleteActivity client activityId = do
  request <- buildRequest methodDelete client resource query
  response <- performRequest client request
  return (if responseStatus response == noContent204
    then Right ()
    else Left (unpack (toStrict (responseBody response))))
 where
  resource = "api/v3/activities/" <> show activityId
  query = [] :: Query

-- | <http://strava.github.io/api/v3/activities/#get-activities>
getCurrentActivities :: Client -> O.GetCurrentActivitiesOptions -> IO (Either String [T.ActivitySummary])
getCurrentActivities client options = get client resource query
 where
  resource = "api/v3/athlete/activities"
  query = toQuery options

-- | <http://strava.github.io/api/v3/activities/#get-feed>
getFeed :: Client -> O.GetFeedOptions -> IO (Either String [T.ActivitySummary])
getFeed client options = get client resource query
 where
  resource = "api/v3/activities/following"
  query = toQuery options

-- | <http://strava.github.io/api/v3/activities/#zones>
getActivityZones :: Client -> Integer -> IO (Either String [T.ActivityZoneDetailed])
getActivityZones client activityId = get client resource query
 where
  resource = "api/v3/activities/" <> show activityId <> "/zones"
  query = [] :: Query

-- | <http://strava.github.io/api/v3/activities/#laps>
getActivityLaps :: Client -> Integer -> IO (Either String [T.ActivityLapSummary])
getActivityLaps client activityId = get client resource query
 where
  resource = "api/v3/activities/" <> show activityId <> "/laps"
  query = [] :: Query

-- * Comments

-- | <http://strava.github.io/api/v3/comments/#list>
getActivityComments :: Client -> Integer -> O.GetActivityCommentsOptions -> IO (Either String [T.CommentSummary])
getActivityComments client activityId options = get client resource query
 where
  resource = "api/v3/activities/" <> show activityId <> "/comments"
  query = toQuery options

-- * Kudos

-- | <http://strava.github.io/api/v3/kudos/#list>
getActivityKudoers :: Client -> Integer -> O.GetActivityKudoersOptions -> IO (Either String [T.AthleteSummary])
getActivityKudoers client activityId options = get client resource query
 where
  resource = "api/v3/activities/" <> show activityId <> "/kudos"
  query = toQuery options

-- * Photos

-- | <http://strava.github.io/api/v3/photos/#list>
getActivityPhotos :: Client -> Integer -> IO (Either String [T.PhotoSummary])
getActivityPhotos client activityId = get client resource query
  where
    resource = "api/v3/activities/" <> show activityId <> "/photos"
    query = [] :: Query

-- * Clubs

-- | <http://strava.github.io/api/v3/clubs/#get-details>
getClub :: Client -> Integer -> IO (Either String T.ClubDetailed)
getClub client clubId = get client resource query
 where
  resource = "api/v3/clubs/" <> show clubId
  query = [] :: Query

-- | <http://strava.github.io/api/v3/clubs/#get-athletes>
getCurrentClubs :: Client -> IO (Either String [T.ClubSummary])
getCurrentClubs client = get client resource query
  where
    resource = "api/v3/athlete/clubs"
    query = [] :: Query

-- | <http://strava.github.io/api/v3/clubs/#get-members>
getClubMembers :: Client -> Integer -> O.GetClubMembersOptions -> IO (Either String [T.AthleteSummary])
getClubMembers client clubId options = get client resource query
  where
    resource = "api/v3/clubs/" <> show clubId <> "/members"
    query = toQuery options

-- | <http://strava.github.io/api/v3/clubs/#get-activities>
getClubActivities :: Client -> Integer -> O.GetClubActivitiesOptions -> IO (Either String [T.ActivitySummary])
getClubActivities client clubId options = get client resource query
 where
  resource = "api/v3/clubs/" <> show clubId <> "/activities"
  query = toQuery options

-- * Gear

-- | <http://strava.github.io/api/v3/gear/#show>
getGear :: Client -> String -> IO (Either String T.GearDetailed)
getGear client gearId = get client resource query
 where
  resource = "api/v3/gear/" <> gearId
  query = [] :: Query

-- * Segments

-- | <http://strava.github.io/api/v3/segments/#retrieve>
getSegment :: Client -> Integer -> IO (Either String T.SegmentDetailed)
getSegment client segmentId = get client resource query
 where
  resource = "api/v3/segments/" <> show segmentId
  query = [] :: Query

-- | <http://strava.github.io/api/v3/segments/#starred>
getStarredSegments :: Client -> O.GetStarredSegmentsOptions -> IO (Either String [T.SegmentSummary])
getStarredSegments client options = get client resource query
 where
  resource = "api/v3/segments/starred"
  query = toQuery options

-- | <http://strava.github.io/api/v3/segments/#efforts>
getSegmentEfforts :: Client -> Integer -> O.GetSegmentEffortsOptions -> IO (Either String [T.EffortDetailed])
getSegmentEfforts client segmentId options = get client resource query
 where
  resource = "api/v3/segments/" <> show segmentId <> "/all_efforts"
  query = toQuery options

-- | <http://strava.github.io/api/v3/segments/#leaderboard>
getSegmentLeaderboard :: Client -> Integer -> O.GetSegmentLeaderboardOptions -> IO (Either String T.SegmentLeaderboardResponse)
getSegmentLeaderboard client segmentId options = get client resource query
 where
  resource = "api/v3/segments/" <> show segmentId <> "/leaderboard"
  query = toQuery options

-- | <http://strava.github.io/api/v3/segments/#explore>
exploreSegments :: Client -> (Double, Double, Double, Double) -> O.ExploreSegmentsOptions -> IO (Either String T.SegmentExplorerResponse)
exploreSegments client (south, west, north, east) options = get client resource query
 where
  resource = "api/v3/segments/explore"
  query = toQuery
    [ ("bounds", intercalate "," (fmap show [south, west, north, east]))
    ] <> toQuery options

-- * Segment Efforts

-- | <http://strava.github.io/api/v3/efforts/#retrieve>
getSegmentEffort :: Client -> Integer -> IO (Either String T.EffortDetailed)
getSegmentEffort client effortId = get client resource query
 where
  resource = "api/v3/segment_efforts/" <> show effortId
  query = [] :: Query

-- * Streams

-- | <http://strava.github.io/api/v3/streams/#activity>
getActivityStreams :: Client -> Integer -> [String] -> O.GetStreamsOptions -> IO (Either String [T.StreamDetailed])
getActivityStreams = flip getStreams "activities"

-- | <http://strava.github.io/api/v3/streams/#effort>
getEffortStreams :: Client -> Integer -> [String] -> O.GetStreamsOptions -> IO (Either String [T.StreamDetailed])
getEffortStreams = flip getStreams "segment_efforts"

-- | <http://strava.github.io/api/v3/streams/#segment>
getSegmentStreams :: Client -> Integer -> [String] -> O.GetStreamsOptions -> IO (Either String [T.StreamDetailed])
getSegmentStreams = flip getStreams "segments"

getStreams :: FromJSON a => Client -> String -> Integer -> [String] -> O.GetStreamsOptions -> IO (Either String a)
getStreams client kind id types options = get client resource query
  where
    resource = concat
        [ "api/v3/"
        , kind
        , "/"
        , show id
        , "/streams/"
        , intercalate "," types
        ]
    query = toQuery options

-- * Uploads

-- | <http://strava.github.io/api/v3/uploads/#post-file>
uploadActivity :: Client -> ByteString -> String -> O.UploadActivityOptions -> IO (Either String T.UploadStatus)
uploadActivity client body dataType options = do
  initialRequest <- buildRequest methodPost client resource query
  let request = initialRequest
        { requestBody = RequestBodyBS body
        }
  response <- performRequest client request
  return (decodeValue response)
 where
  resource = "api/v3/uploads"
  query = toQuery
    [ ("data_type", dataType)
    ] <> toQuery options

-- | <http://strava.github.io/api/v3/uploads/#get-status>
getUpload :: Client -> Integer -> IO (Either String T.UploadStatus)
getUpload client uploadId = get client resource query
 where
  resource = "api/v3/uploads/" <> show uploadId
  query = [] :: Query
