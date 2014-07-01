# [Strive][1]

[![Build Status](https://travis-ci.org/tfausak/strive.svg?branch=master)](https://travis-ci.org/tfausak/strive)

A Haskell client for the [Strava V3 API][2].

-   [Usage](#usage)
    -   [Authentication](#authentication)
        -   [Request access](#request-access)
        -   [Token exchange](#token-exchange)
        -   [Deauthorization](#deauthorization)
    -   [Athletes](#athletes)
        -   [Retrieve current athlete](#retrieve-current-athlete)
        -   [Retrieve another athlete](#retrieve-another-athlete)
        -   [Update current athlete](#update-current-athlete)
        -   [List athlete K/QOMs/CRs](#list-athlete-kqomscrs)
    -   [Friends and followers](#friends-and-followers)
        -   [List athlete friends](#list-athlete-friends)
        -   [List athlete followers](#list-athlete-followers)
        -   [List both following](#list-both-following)
    -   [Activities](#activities)
        -   [Create an activity](#create-an-activity)
        -   [Retrieve an activity](#retrieve-an-activity)
        -   [Update an activity](#update-an-activity)
        -   [Delete an activity](#delete-an-activity)
        -   [List athlete activities](#list-athlete-activities)
        -   [List friends' activities](#list-friends-activities)
        -   [List activity zones](#list-activity-zones)
        -   [List activity laps](#list-activity-laps)
    -   [Comments](#comments)
        -   [List activity comments](#list-activity-comments)
    -   [Kudos](#kudos)
        -   [List activity kudoers](#list-activity-kudoers)
    -   [Photos](#photos)
        -   [List activity photos](#list-activity-photos)
    -   [Clubs](#clubs)
        -   [Retrieve a club](#retrieve-a-club)
        -   [List athlete clubs](#list-athlete-clubs)
        -   [List club members](#list-club-members)
        -   [List club activities](#list-club-activities)
    -   [Gear](#gear)
        -   [Retrieve gear](#retrieve-gear)
    -   [Segments](#segments)
        -   [Retrieve a segment](#retrieve-a-segment)
        -   [List starred segments](#list-starred-segments)
        -   [List efforts](#list-efforts)
        -   [Segment leaderboard](#segment-leaderboard)
        -   [Segment explorer](#segment-explorer)
        -   [Segment efforts](#segment-efforts)
        -   [Retrieve a segment effort](#retrieve-a-segment-effort)
    -   [Streams](#streams)
        -   [Retrieve activity streams](#retrieve-activity-streams)
        -   [Retrieve effort streams](#retrieve-effort-streams)
        -   [Retrieve segment streams](#retrieve-segment-streams)
    -   [Uploads](#uploads)
        -   [Upload an activity](#upload-an-activity)
        -   [Check upload status](#check-upload-status)

## Usage

To use the API, you'll need an access token. Once you have that, create a new
client using the default HTTP manager.

~~~ {.haskell .ignore}
import Strive
let token = "..."
client <- newClient token
-- Client {..}
~~~

<!--
~~~ {.haskell}
import Data.ByteString.Char8 (pack)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (UTCTime))
import Strive
import System.Exit (exitSuccess)

main :: IO ()
main = do
  _ <- exitSuccess
  client <- newClient ""
~~~
-->

Many of the examples use the same parameters.

~~~ {.haskell}
  let activityId        = 141273622
  let activityType      = Just "riding"
  let after             = UTCTime (fromGregorian 1970 0 0) 0
  let ageGroup          = Just "0_24"
  let approvalPrompt    = Just "force"
  let athleteId         = 65516
  let before            = UTCTime (fromGregorian 2020 0 0) 0
  let bounds            = (32, -96, 33, -95)
  let clientId          = 1790
  let clientSecret      = "..."
  let clubId            = 11193
  let code              = "..."
  let effortId          = 1595370098
  let following         = Just False
  let gearId            = "b387855"
  let gender            = Just 'F'
  let includeAllEfforts = Just True
  let includeMarkdown   = Just False
  let maxCat            = Just 5
  let minCat            = Just 0
  let page              = Just 1
  let perPage           = Just 200
  let range             = Just "this_year"
  let redirectURL       = "http://localhost"
  let resolution        = Just "low"
  let scope             = Just ["view_private", "write"]
  let segmentId         = 4773104
  let seriesType        = Just "time"
  let state             = Nothing
  let streamTypes       = ["time"]
  let weightClass       = Just "55_64"
~~~

### [Authentication](http://strava.github.io/api/v3/oauth/)

#### [Request access](http://strava.github.io/api/v3/oauth/#get-authorize)

~~~ {.haskell}
  let authorizeURL = buildAuthorizeURL clientId redirectURL approvalPrompt scope state
  print authorizeURL
  -- ""https://www.strava.com/oauth/authorize?.."
~~~

#### [Token exchange](http://strava.github.io/api/v3/oauth/#post-token)

~~~ {.haskell}
  response <- postToken clientId clientSecret code
  print response
  -- Right (TokenExchangeResponse {..})
~~~

#### [Deauthorization](http://strava.github.io/api/v3/oauth/#deauthorize)

~~~ {.haskell}
  response' <- postDeauthorize client
  print response'
  -- Right (DeauthorizationResponse {..})
~~~

### [Athletes](http://strava.github.io/api/v3/athlete/)

#### [Retrieve current athlete](http://strava.github.io/api/v3/athlete/#get-details)

~~~ {.haskell}
  currentAthlete <- getCurrentAthlete client
  print currentAthlete
  -- Right (AthleteDetailed {..})
~~~

#### [Retrieve another athlete](http://strava.github.io/api/v3/athlete/#get-another-details)

~~~ {.haskell}
  athlete <- getAthlete client athleteId
  print athlete
  -- Right (AthleteSummary {..})
~~~

#### [Update current athlete](http://strava.github.io/api/v3/athlete/#update)

~~~ {.haskell}
  updatedAthlete <- putCurrentAthlete client Nothing Nothing Nothing Nothing Nothing
  print updatedAthlete
  -- Right (AthleteDetailed {..})
~~~

#### [List athlete K/QOMs/CRs](http://strava.github.io/api/v3/athlete/#koms)

~~~ {.haskell}
  athleteCRs <- getAthleteCRs client athleteId page perPage
  print athleteCRs
  -- Right [EffortSummary {..},..]
~~~

### [Friends and followers](http://strava.github.io/api/v3/follow/)

#### [List athlete friends](http://strava.github.io/api/v3/follow/#friends)

~~~ {.haskell}
  currentFriends <- getCurrentFriends client page perPage
  print currentFriends
  -- Right [AthleteSummary {..},..]
~~~

#### [List athlete followers](http://strava.github.io/api/v3/follow/#followers)

~~~ {.haskell}
  currentFollowers <- getCurrentFollowers client page perPage
  print currentFollowers
  -- Right [AthleteSummary {..},..]
~~~

#### [List both following](http://strava.github.io/api/v3/follow/#both)

~~~ {.haskell}
  commonFriends <- getCommonFriends client athleteId page perPage
  print commonFriends
  -- Right [AthleteSummary {..},..]
~~~

### [Activities](http://strava.github.io/api/v3/activities/)

#### [Create an activity](http://strava.github.io/api/v3/activities/#create)

~~~ {.haskell}
  createdActivity <- postActivity client "" "" after 0 Nothing Nothing
  print createdActivity
  -- Right (AthleteDetailed {..})
~~~

#### [Retrieve an activity](http://strava.github.io/api/v3/activities/#get-details)

~~~ {.haskell}
  activity <- getActivity client activityId includeAllEfforts
  print activity
  -- Right (ActivitySummary {..})
~~~

#### [Update an activity](http://strava.github.io/api/v3/activities/#put-updates)

~~~ {.haskell}
  updatedActivity <- putActivity client activityId Nothing Nothing Nothing Nothing Nothing Nothing Nothing
  print updatedActivity
  -- Right (ActivityDetailed {..})
~~~

#### [Delete an activity](http://strava.github.io/api/v3/activities/#delete)

~~~ {.haskell}
  nothing <- deleteActivity client activityId
  print nothing
  -- Right (Null)
~~~

#### [List athlete activities](http://strava.github.io/api/v3/activities/#get-activities)

~~~ {.haskell}
  currentActivities <- getCurrentActivities client (Just before) (Just after) page perPage
  print currentActivities
  -- Right [ActivitySummary {..},..]
~~~

#### [List friends' activities](http://strava.github.io/api/v3/activities/#get-feed)

~~~ {.haskell}
  feed <- getFeed client page perPage
  print feed
  -- Right [ActivitySummary {..},..]
~~~

#### [List activity zones](http://strava.github.io/api/v3/activities/#zones)

~~~ {.haskell}
  activityZones <- getActivityZones client activityId
  print activityZones
  -- Right [ActivityZoneDetailed {..},..]
~~~

#### [List activity laps](http://strava.github.io/api/v3/activities/#laps)

~~~ {.haskell}
  activityLaps <- getActivityLaps client activityId
  print activityLaps
  -- Right [ActivityZoneDetailed {..},..]
~~~

### [Comments](http://strava.github.io/api/v3/comments/)

#### [List activity comments](http://strava.github.io/api/v3/comments/#list)

~~~ {.haskell}
  activityComments <- getActivityComments client activityId includeMarkdown page perPage
  print activityComments
  -- Right [CommentSummary {..},..]
~~~

### [Kudos](http://strava.github.io/api/v3/kudos/)

#### [List activity kudoers](http://strava.github.io/api/v3/kudos/#list)

~~~ {.haskell}
  activityKudoers <- getActivityKudoers client activityId page perPage
  print activityKudoers
  -- Right [AthleteSummary {..},..]
~~~

### [Photos](http://strava.github.io/api/v3/photos/)

#### [List activity photos](http://strava.github.io/api/v3/photos/#list)

~~~ {.haskell}
  activityPhotos <- getActivityPhotos client activityId
  print activityPhotos
  -- Right [PhotoSummary {..},..]
~~~

### [Clubs](http://strava.github.io/api/v3/clubs/)

#### [Retrieve a club](http://strava.github.io/api/v3/clubs/#get-details)

~~~ {.haskell}
  club <- getClub client clubId
  print club
  -- Right (ClubDetailed {..})
~~~

#### [List athlete clubs](http://strava.github.io/api/v3/clubs/#get-athletes)

~~~ {.haskell}
  currentClubs <- getCurrentClubs client
  print currentClubs
  -- Right [ClubSummary {..},..]
~~~

#### [List club members](http://strava.github.io/api/v3/clubs/#get-members)

~~~ {.haskell}
  clubMembers <- getClubMembers client clubId page perPage
  print clubMembers
  -- Right [AthleteSummary {..},..]
~~~

#### [List club activities](http://strava.github.io/api/v3/clubs/#get-activities)

~~~ {.haskell}
  clubActivities <- getClubActivities client clubId page perPage
  print clubActivities
  -- Right [ActivitySummary {..},..]
~~~

### [Gear](http://strava.github.io/api/v3/gear/)

#### [Retrieve gear](http://strava.github.io/api/v3/gear/#show)

~~~ {.haskell}
  gear <- getGear client gearId
  print gear
  -- Right (GearDetailed {..})
~~~

### [Segments](http://strava.github.io/api/v3/segments/)

#### [Retrieve a segment](http://strava.github.io/api/v3/segments/#retrieve)

~~~ {.haskell}
  segment <- getSegment client segmentId
  print segment
  -- Right (SegmentDetailed {..})
~~~

#### [List starred segments](http://strava.github.io/api/v3/segments/#starred)

~~~ {.haskell}
  starredSegments <- getStarredSegments client page perPage
  print starredSegments
  -- Right [SegmentSummary {..},..]
~~~

#### [List efforts](http://strava.github.io/api/v3/segments/#efforts)

~~~ {.haskell}
  efforts <- getSegmentEfforts client segmentId (Just athleteId) (Just (after, before)) page perPage
  print efforts
  -- Right [EffortSummary {..},..]
~~~

#### [Segment leaderboard](http://strava.github.io/api/v3/segments/#leaderboard)

~~~ {.haskell}
  segmentLeaders <- getSegmentLeaderboard client segmentId gender ageGroup weightClass following (Just clubId) range page perPage
  print segmentLeaders
  -- Right [SegmentLeaderboardEntry {..},..]
~~~

#### [Segment explorer](http://strava.github.io/api/v3/segments/#explore)

~~~ {.haskell}
  segments <- exploreSegments client bounds activityType minCat maxCat
  print segments
  -- Right [SegmentExplorerEntry {..},..]
~~~

### [Segment efforts](http://strava.github.io/api/v3/efforts/)

#### [Retrieve a segment effort](http://strava.github.io/api/v3/efforts/#retrieve)

~~~ {.haskell}
  effort <- getEffort client effortId
  print effort
  -- Right (EffortSummary {..})
~~~

### [Streams](http://strava.github.io/api/v3/streams/)

#### [Retrieve activity streams](http://strava.github.io/api/v3/streams/#activity)

~~~ {.haskell}
  activityStreams <- getActivityStreams client activityId streamTypes resolution seriesType
  print activityStreams
  -- Right [StreamDetailed {..},..]
~~~

#### [Retrieve effort streams](http://strava.github.io/api/v3/streams/#effort)

~~~ {.haskell}
  effortStreams <- getEffortStreams client effortId streamTypes resolution seriesType
  print effortStreams
  -- Right [StreamDetailed {..},..]
~~~

#### [Retrieve segment streams](http://strava.github.io/api/v3/streams/#segment)

~~~ {.haskell}
  segmentStreams <- getSegmentStreams client segmentId streamTypes resolution seriesType
  print segmentStreams
  -- Right [StreamDetailed {..},..]
~~~

### [Uploads](http://strava.github.io/api/v3/uploads/)

#### [Upload an activity](http://strava.github.io/api/v3/uploads/#post-file)

~~~ {.haskell}
  postedUpload <- postUpload client (pack "...") "gpx.gz" Nothing Nothing Nothing Nothing Nothing Nothing
  print postedUpload
  -- Right (UploadDetailed {..})
~~~

#### [Check upload status](http://strava.github.io/api/v3/uploads/#get-status)

~~~ {.haskell}
  upload <- getUpload client 123
  print upload
  -- Right (UploadDetailed {..})
~~~

[1]: https://github.com/tfausak/strive
[2]: http://strava.github.io/api/
