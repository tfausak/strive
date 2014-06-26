# [Strive][1]

A Haskell client for the [Strava V3 API][2].

## Installation

This project uses [Semantic Versioning][3].

~~~ {.sh}
$ cabal update
$ cabal install strive-0.1.0
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
import           Data.Time.Calendar (fromGregorian)
import           Data.Time.Clock    (UTCTime (UTCTime), getCurrentTime)
import           Strive
import           System.Environment (getArgs)

main :: IO ()
main = do
    (token : _) <- getArgs
    client <- newClient token
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
    let athleteId = 65516

    athlete <- getAthlete client athleteId
    print athlete
    -- Right (AthleteSummary {..})
~~~

#### Update Current Athlete

<https://github.com/tfausak/strive/issues/7>

#### List Athlete K/QOMs/CRs

~~~ {.haskell}
    let athleteId = 65516
    let page      = Just 1
    let perPage   = Just 200

    athleteCRs <- getAthleteCRs client athleteId page perPage
    print athleteCRs
    -- Right [EffortSummary {..},..]
~~~

### Friends and Followers

#### List Athlete Friends

~~~ {.haskell}
    let page    = Just 1
    let perPage = Just 200

    currentFriends <- getCurrentFriends client page perPage
    print currentFriends
    -- Right [AthleteSummary {..},..]
~~~

#### List Athlete Followers

~~~ {.haskell}
    let page    = Just 1
    let perPage = Just 200

    currentFollowers <- getCurrentFollowers client page perPage
    print currentFollowers
    -- Right [AthleteSummary {..},..]
~~~

#### List Both Following

~~~ {.haskell}
    let athleteId = 65516
    let page      = Just 1
    let perPage   = Just 200

    commonFriends <- getCommonFriends client athleteId page perPage
    print commonFriends
    -- Right [AthleteSummary {..},..]
~~~

### Activities

#### Create an Activity

<https://github.com/tfausak/strive/issues/12>

#### Retrieve an Activity

~~~ {.haskell}
    let activityId        = 141273622
    let includeAllEfforts = Just True

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
    time <- getCurrentTime
    let before  = Just time
    let after   = Nothing
    let page    = Just 1
    let perPage = Just 200

    currentActivities <- getCurrentActivities client before after page perPage
    print currentActivities
    -- Right [ActivitySummary {..},..]
~~~

#### List Friends' Activities

~~~ {.haskell}
    let page    = Just 1
    let perPage = Just 200

    feed <- getFeed client page perPage
    print feed
    -- Right [ActivitySummary {..},..]
~~~

#### List Activity Zones

~~~ {.haskell}
    let activityId = 141273622

    activityZones <- getActivityZones client activityId
    print activityZones
    -- Right [ZoneSummary {..},..]
~~~

#### List Activity Laps

~~~ {.haskell}
    let activityId = 141273622

    activityLaps <- getActivityLaps client activityId
    print activityLaps
    -- Right [ZoneSummary {..},..]
~~~

### Comments

#### List Activity Comments

~~~ {.haskell}
    let activityId      = 42001703
    let includeMarkdown = Just False
    let page            = Just 1
    let perPage         = Just 200

    activityComments <- getActivityComments client activityId includeMarkdown page perPage
    print activityComments
    -- Right [CommentSummary {..},..]
~~~

### Kudos

#### List Activity Kudoers

~~~ {.haskell}
    let activityId = 141273622
    let page       = Just 1
    let perPage    = Just 200

    activityKudoers <- getActivityKudoers client activityId page perPage
    print activityKudoers
    -- Right [AthleteSummary {..},..]
~~~

### Photos

#### List Activity Photos

~~~ {.haskell}
    let activityId = 141273622

    activityPhotos <- getActivityPhotos client activityId
    print activityPhotos
    -- Right [PhotoSummary {..},..]
~~~

### Clubs

#### Retrieve a Club

~~~ {.haskell}
    let clubId = 11193

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
    let clubId  = 11193
    let page    = Just 1
    let perPage = Just 200

    clubMembers <- getClubMembers client clubId page perPage
    print clubMembers
    -- Right [AthleteSummary {..},..]
~~~

#### List Club Activities

~~~ {.haskell}
    let clubId  = 11193
    let page    = Just 1
    let perPage = Just 200

    clubActivities <- getClubActivities client clubId page perPage
    print clubActivities
    -- Right [ActivitySummary {..},..]
~~~

### Gear

#### Retrieve Gear

~~~ {.haskell}
    let gearId = "b387855"

    gear <- getGear client gearId
    print gear
    -- Right (GearDetailed {..})
~~~

### Segments

#### Retrieve a Segment

~~~ {.haskell}
    let segmentId = 4773104

    segment <- getSegment client segmentId
    print segment
    -- Right (SegmentDetailed {..})
~~~

#### List Starred Segments

~~~ {.haskell}
    let page    = Just 1
    let perPage = Just 200

    starredSegments <- getStarredSegments client page perPage
    print starredSegments
    -- Right [SegmentSummary {..},..]
~~~

#### List Efforts

~~~ {.haskell}
    time <- getCurrentTime
    let after     = UTCTime (fromGregorian 1970 0 0) 0
    let before    = time
    let segmentId = 4773104
    let range     = Just (after, before)
    let page      = Just 1
    let perPage   = Just 200

    efforts <- getSegmentEfforts client segmentId range page perPage
    print efforts
    -- Right [EffortSummary {..},..]
~~~

#### Segment Leaderboard

~~~ {.haskell}
    let segmentId   = 1091029
    let gender      = Nothing
    let ageGroup    = Just "0_24"
    let weightClass = Just "55_64"
    let following   = Just False
    let clubId      = Nothing
    let range       = Just "this_year"
    let page        = Just 1
    let perPage     = Just 200

    segmentLeaders <- getSegmentLeaderboard client segmentId gender ageGroup weightClass following clubId range page perPage
    print segmentLeaders
    -- Right [SegmentLeader {..},..]
~~~

#### Segment Explorer

~~~ {.haskell}
    let south        = 32.0
    let west         = -96.0
    let north        = 33.0
    let east         = -95.0
    let activityType = Just "riding"
    let minCat       = Just 0
    let maxCat       = Just 5

    segments <- exploreSegments client (south, west, north, east) activityType minCat maxCat
    print segments
    -- Right [SegmentExploration {..},..]
~~~

### Segment Efforts

#### Retrieve a Segment Effort

~~~ {.haskell}
    let effortId = 1595370098

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
