<h1><a href="https://github.com/tfausak/strive">Strive</a></h1>

A Haskell client for the [Strava V3 API][2].

<h2>Installation</h2>

This project uses [Semantic Versioning][3].

``` sh
$ cabal install strive-0.1.0
```

<h2>Usage</h2>

To use the API, you'll need an access token. Once you have that, create a new
client using the default HTTP manager.

``` hs
import Strive
let token = "..."
client <- newClient token
-- Client {..}
```

Note: This README is executable. Run it with this command.

``` sh
$ runhaskell README.lhs ACCESS_TOKEN
```

Since it's executable, there's some necessary boilerplate.

> module README (main) where
> import Data.Time.Calendar (fromGregorian)
> import Data.Time.Clock (UTCTime (UTCTime), getCurrentTime)
> import Strive
> import System.Environment (getArgs)
> main :: IO ()
> main = do
>     (token : _) <- getArgs
>     client <- newClient token

<h3>Authentication</h3>

<h4>Request Access</h4>

<https://github.com/tfausak/strive/issues/36>

<h4>Token Exchange</h4>

<https://github.com/tfausak/strive/issues/37>

<h4>Deauthorization</h4>

<https://github.com/tfausak/strive/issues/38>

<h3>Athletes</h3>

<h4>Retrieve Current Athlete</h4>

>     currentAthlete <- getCurrentAthlete client
>     print currentAthlete
>     -- Right (AthleteDetailed {..})

<h4>Retrieve Another Athlete</h4>

>     let athleteId = 65516
>     athlete <- getAthlete client athleteId
>     print athlete
>     -- Right (AthleteSummary {..})

<h4>Update Current Athlete</h4>

<https://github.com/tfausak/strive/issues/7>

<h4>List Athlete K/QOMs/CRs</h4>

>     let athleteId = 65516
>     let page = Just 1
>     let perPage = Just 200
>     athleteCRs <- getAthleteCRs client athleteId page perPage
>     print athleteCRs
>     -- Right [EffortSummary {..},..]

<h3>Friends and Followers</h3>

<h4>List Athlete Friends</h4>

>     let page = Just 1
>     let perPage = Just 200
>     currentFriends <- getCurrentFriends client page perPage
>     print currentFriends
>     -- Right [AthleteSummary {..},..]

<h4>List Athlete Followers</h4>

>     let page = Just 1
>     let perPage = Just 200
>     currentFollowers <- getCurrentFollowers client page perPage
>     print currentFollowers
>     -- Right [AthleteSummary {..},..]

<h4>List Both Following</h4>

>     let athleteId = 65516
>     let page = Just 1
>     let perPage = Just 200
>     commonFriends <- getCommonFriends client athleteId page perPage
>     print commonFriends
>     -- Right [AthleteSummary {..},..]

<h3>Activities</h3>

<h4>Create an Activity</h4>

<https://github.com/tfausak/strive/issues/12>

<h4>Retrieve an Activity</h4>

>     let activityId = 141273622
>     let includeAllEfforts = Just True
>     activity <- getActivity client activityId includeAllEfforts
>     print activity
>     -- Right (ActivitySummary {..})

<h4>Update an Activity</h4>

<https://github.com/tfausak/strive/issues/14>

<h4>Delete an Activity</h4>

<https://github.com/tfausak/strive/issues/15>

<h4>List Athlete Activities</h4>

>     time <- getCurrentTime
>     let before = Just time
>     let after = Nothing
>     let page = Just 1
>     let perPage = Just 200
>     currentActivities <- getCurrentActivities client before after page perPage
>     print currentActivities
>     -- Right [ActivitySummary {..},..]

<h4>List Friends' Activities</h4>

>     let page = Just 1
>     let perPage = Just 200
>     feed <- getFeed client page perPage
>     print feed
>     -- Right [ActivitySummary {..},..]

<h4>List Activity Zones</h4>

>     let activityId = 141273622
>     activityZones <- getActivityZones client activityId
>     print activityZones
>     -- Right [ZoneSummary {..},..]

<h4>List Activity Laps</h4>

>     let activityId = 141273622
>     activityLaps <- getActivityLaps client activityId
>     print activityLaps
>     -- Right [ZoneSummary {..},..]

<h3>Comments</h3>

<h4>List Activity Comments</h4>

>     let activityId = 42001703
>     let includeMarkdown = Just False
>     let page = Just 1
>     let perPage = Just 200
>     activityComments <- getActivityComments client activityId includeMarkdown page perPage
>     print activityComments
>     -- Right [CommentSummary {..},..]

<h3>Kudos</h3>

<h4>List Activity Kudoers</h4>

>     let activityId = 141273622
>     let page = Just 1
>     let perPage = Just 200
>     activityKudoers <- getActivityKudoers client activityId page perPage
>     print activityKudoers
>     -- Right [AthleteSummary {..},..]

<h3>Photos</h3>

<h4>List Activity Photos</h4>

>     let activityId = 141273622
>     activityPhotos <- getActivityPhotos client activityId
>     print activityPhotos
>     -- Right [PhotoSummary {..},..]

<h3>Clubs</h3>

<h4>Retrieve a Club</h4>

>     let clubId = 11193
>     club <- getClub client clubId
>     print club
>     -- Right (ClubDetailed {..})

<h4>List Athlete Clubs</h4>

>     currentClubs <- getCurrentClubs client
>     print currentClubs
>     -- Right [ClubSummary {..},..]

<h4>List Club Members</h4>

>     let clubId = 11193
>     let page = Just 1
>     let perPage = Just 200
>     clubMembers <- getClubMembers client clubId page perPage
>     print clubMembers
>     -- Right [AthleteSummary {..},..]

<h4>List Club Activities</h4>

>     let clubId = 11193
>     let page = Just 1
>     let perPage = Just 200
>     clubActivities <- getClubActivities client clubId page perPage
>     print clubActivities
>     -- Right [ActivitySummary {..},..]

<h3>Gear</h3>

<h4>Retrieve Gear</h4>

>     let gearId = "b387855"
>     gear <- getGear client gearId
>     print gear
>     -- Right (GearDetailed {..})

<h3>Segments</h3>

<h4>Retrieve a Segment</h4>

>     let segmentId = 4773104
>     segment <- getSegment client segmentId
>     print segment
>     -- Right (SegmentDetailed {..})

<h4>List Starred Segments</h4>

>     let page = Just 1
>     let perPage = Just 200
>     starredSegments <- getStarredSegments client page perPage
>     print starredSegments
>     -- Right [SegmentSummary {..},..]

<h4>List Efforts</h4>

>     time <- getCurrentTime
>     let after = UTCTime (fromGregorian 1970 0 0) 0
>     let before = time
>     let segmentId = 4773104
>     let range = Just (after, before)
>     let page = Just 1
>     let perPage = Just 200
>     efforts <- getSegmentEfforts client segmentId range page perPage
>     print efforts
>     -- Right [EffortSummary {..},..]

<h4>Segment Leaderboard</h4>

>     let segmentId = 1091029
>     let gender = Nothing
>     let ageGroup = Just "0_24"
>     let weightClass = Just "55_64"
>     let following = Just False
>     let clubId = Nothing
>     let range = Just "this_year"
>     let page = Just 1
>     let perPage = Just 200
>     segmentLeaders <- getSegmentLeaderboard client segmentId gender ageGroup weightClass following clubId range page perPage
>     print segmentLeaders
>     -- Right [SegmentLeader {..},..]

<h4>Segment Explorer</h4>

>     let south = 32.0
>     let west = -96.0
>     let north = 33.0
>     let east = -95.0
>     let activityType = Just "riding"
>     let minCat = Just 0
>     let maxCat = Just 5
>     segments <- exploreSegments client (south, west, north, east) activityType minCat maxCat
>     print segments
>     -- Right [SegmentExploration {..},..]

<h3>Segment Efforts</h3>

<h4>Retrieve a Segment Effort</h4>

>     let effortId = 1595370098
>     effort <- getEffort client effortId
>     print effort
>     -- Right (EffortSummary {..})

<h3>Streams</h3>

<h4>Retrieve Activity Streams</h4>

<https://github.com/tfausak/strive/issues/31>

<h4>Retrieve Effort Streams</h4>

<https://github.com/tfausak/strive/issues/32>

<h4>Retrieve Segment Streams</h4>

<https://github.com/tfausak/strive/issues/33>

<h3>Uploads</h3>

<h4>Upload an Activity</h4>

<https://github.com/tfausak/strive/issues/34>

<h4>Check Upload Status</h4>

<https://github.com/tfausak/strive/issues/35>

[1]: https://github.com/tfausak/strive
[2]: http://strava.github.io/api/
[3]: http://semver.org/spec/v2.0.0.html
