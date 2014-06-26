# [Strive][1]

A Haskell client for the [Strava V3 API][2].

## Installation

This project uses [Semantic Versioning][3].

~~~ {.sh}
$ cabal update
$ cabal install strive-0.1.1
~~~

## Usage

To use the API, you'll need an access token. Once you have that, create a new
client using the default HTTP manager.

~~~ {.haskell .ignore}
import Strive
let token = "..."
client <- newClient token
-- Client {..}
~~~

Note: This file is executable. Compile and run it with these commands.

~~~ {.sh}
$ cabal exec ghc -- -pgmL markdown-unlit -x lhs README.md
$ ./README ACCESS_TOKEN
~~~

Since it's executable, there's some necessary boilerplate.

~~~ {.haskell}
import           Data.Maybe         (fromMaybe, listToMaybe)
import           Data.Time.Calendar (fromGregorian)
import           Data.Time.Clock    (UTCTime (UTCTime), getCurrentTime)
import           Strive
import           System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    let token = fromMaybe "" (listToMaybe args)
    client <- newClient token
~~~

Many of the examples use the same parameters.

~~~ {.haskell}
    time <- getCurrentTime

    let activityId        = 141273622
    let activityType      = Just "riding"
    let after             = UTCTime (fromGregorian 1970 0 0) 0
    let ageGroup          = Just "0_24"
    let athleteId         = 65516
    let before            = time
    let clubId            = 11193
    let dateRange         = Just "this_year"
    let east              = -95.0
    let effortId          = 1595370098
    let following         = Just False
    let gearId            = "b387855"
    let gender            = Just 'F'
    let includeAllEfforts = Just True
    let includeMarkdown   = Just False
    let maxCat            = Just 5
    let maybeAfter        = Just after
    let maybeAthleteId    = Just athleteId
    let maybeBefore       = Just time
    let maybeClubId       = Just clubId
    let minCat            = Just 0
    let north             = 33.0
    let page              = Just 1
    let perPage           = Just 200
    let range             = Just (after, before)
    let segmentId         = 4773104
    let south             = 32.0
    let weightClass       = Just "55_64"
    let west              = -96.0
~~~

### Authentication

#### Request Access

<https://github.com/tfausak/strive/issues/36>

#### Token Exchange

<https://github.com/tfausak/strive/issues/37>

#### Deauthorization

<https://github.com/tfausak/strive/issues/38>

### Athletes

#### Retrieve Current Athlete

~~~ {.haskell}
    currentAthlete <- getCurrentAthlete client
    print currentAthlete
    -- Right (AthleteDetailed {..})
~~~

#### Retrieve Another Athlete

~~~ {.haskell}
    athlete <- getAthlete client athleteId
    print athlete
    -- Right (AthleteSummary {..})
~~~

#### Update Current Athlete

<https://github.com/tfausak/strive/issues/7>

#### List Athlete K/QOMs/CRs

~~~ {.haskell}
    athleteCRs <- getAthleteCRs client athleteId page perPage
    print athleteCRs
    -- Right [EffortSummary {..},..]
~~~

### Friends and Followers

#### List Athlete Friends

~~~ {.haskell}
    currentFriends <- getCurrentFriends client page perPage
    print currentFriends
    -- Right [AthleteSummary {..},..]
~~~

#### List Athlete Followers

~~~ {.haskell}
    currentFollowers <- getCurrentFollowers client page perPage
    print currentFollowers
    -- Right [AthleteSummary {..},..]
~~~

#### List Both Following

~~~ {.haskell}
    commonFriends <- getCommonFriends client athleteId page perPage
    print commonFriends
    -- Right [AthleteSummary {..},..]
~~~

### Activities

#### Create an Activity

<https://github.com/tfausak/strive/issues/12>

#### Retrieve an Activity

~~~ {.haskell}
    activity <- getActivity client activityId includeAllEfforts
    print activity
    -- Right (ActivitySummary {..})
~~~

#### Update an Activity

<https://github.com/tfausak/strive/issues/14>

#### Delete an Activity

<https://github.com/tfausak/strive/issues/15>

#### List Athlete Activities

~~~ {.haskell}
    currentActivities <- getCurrentActivities client maybeBefore maybeAfter page perPage
    print currentActivities
    -- Right [ActivitySummary {..},..]
~~~

#### List Friends' Activities

~~~ {.haskell}
    feed <- getFeed client page perPage
    print feed
    -- Right [ActivitySummary {..},..]
~~~

#### List Activity Zones

~~~ {.haskell}
    activityZones <- getActivityZones client activityId
    print activityZones
    -- Right [ZoneSummary {..},..]
~~~

#### List Activity Laps

~~~ {.haskell}
    activityLaps <- getActivityLaps client activityId
    print activityLaps
    -- Right [ZoneSummary {..},..]
~~~

### Comments

#### List Activity Comments

~~~ {.haskell}
    activityComments <- getActivityComments client activityId includeMarkdown page perPage
    print activityComments
    -- Right [CommentSummary {..},..]
~~~

### Kudos

#### List Activity Kudoers

~~~ {.haskell}
    activityKudoers <- getActivityKudoers client activityId page perPage
    print activityKudoers
    -- Right [AthleteSummary {..},..]
~~~

### Photos

#### List Activity Photos

~~~ {.haskell}
    activityPhotos <- getActivityPhotos client activityId
    print activityPhotos
    -- Right [PhotoSummary {..},..]
~~~

### Clubs

#### Retrieve a Club

~~~ {.haskell}
    club <- getClub client clubId
    print club
    -- Right (ClubDetailed {..})
~~~

#### List Athlete Clubs

~~~ {.haskell}
    currentClubs <- getCurrentClubs client
    print currentClubs
    -- Right [ClubSummary {..},..]
~~~

#### List Club Members

~~~ {.haskell}
    clubMembers <- getClubMembers client clubId page perPage
    print clubMembers
    -- Right [AthleteSummary {..},..]
~~~

#### List Club Activities

~~~ {.haskell}
    clubActivities <- getClubActivities client clubId page perPage
    print clubActivities
    -- Right [ActivitySummary {..},..]
~~~

### Gear

#### Retrieve Gear

~~~ {.haskell}
    gear <- getGear client gearId
    print gear
    -- Right (GearDetailed {..})
~~~

### Segments

#### Retrieve a Segment

~~~ {.haskell}
    segment <- getSegment client segmentId
    print segment
    -- Right (SegmentDetailed {..})
~~~

#### List Starred Segments

~~~ {.haskell}
    starredSegments <- getStarredSegments client page perPage
    print starredSegments
    -- Right [SegmentSummary {..},..]
~~~

#### List Efforts

~~~ {.haskell}
    efforts <- getSegmentEfforts client segmentId maybeAthleteId range page perPage
    print efforts
    -- Right [EffortSummary {..},..]
~~~

#### Segment Leaderboard

~~~ {.haskell}
    segmentLeaders <- getSegmentLeaderboard client segmentId gender ageGroup weightClass following maybeClubId dateRange page perPage
    print segmentLeaders
    -- Right [SegmentLeader {..},..]
~~~

#### Segment Explorer

~~~ {.haskell}
    segments <- exploreSegments client (south, west, north, east) activityType minCat maxCat
    print segments
    -- Right [SegmentExploration {..},..]
~~~

### Segment Efforts

#### Retrieve a Segment Effort

~~~ {.haskell}
    effort <- getEffort client effortId
    print effort
    -- Right (EffortSummary {..})
~~~

### Streams

#### Retrieve Activity Streams

<https://github.com/tfausak/strive/issues/31>

#### Retrieve Effort Streams

<https://github.com/tfausak/strive/issues/32>

#### Retrieve Segment Streams

<https://github.com/tfausak/strive/issues/33>

### Uploads

#### Upload an Activity

<https://github.com/tfausak/strive/issues/34>

#### Check Upload Status

<https://github.com/tfausak/strive/issues/35>

[1]: https://github.com/tfausak/strive
[2]: http://strava.github.io/api/
[3]: http://semver.org/spec/v2.0.0.html
