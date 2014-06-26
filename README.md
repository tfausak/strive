# [Strive][1]

A Haskell client for the [Strava V3 API][2].

## Installation

This project uses [Semantic Versioning][3].

``` sh
$ cabal install strive-0.1.0
```

## Usage

To use the API, you'll need an access token. Once you have that, create a new
client using the default HTTP manager.

``` hs
import Strive
let token = "..."
client <- newClient token
-- Client {..}
```

### Authentication

#### Request Access

<https://github.com/tfausak/strive/issues/36>

#### Token Exchange

<https://github.com/tfausak/strive/issues/37>

#### Deauthorization

<https://github.com/tfausak/strive/issues/38>

### Athletes

#### Retrieve Current Athlete

``` hs
getCurrentAthlete client
-- Right (AthleteDetailed {..})
```

#### Retrieve Another Athlete

``` hs
let athleteId = 65516
getAthlete client athleteId
-- Right (AthleteSummary {..})
```

#### Update Current Athlete

<https://github.com/tfausak/strive/issues/7>

#### List Athlete K/QOMs/CRs

``` hs
let athleteId = 65516
let page = Just 1
let perPage = Just 200
getAthleteCRs client athleteId page perPage
-- Right [EffortSummary {..},..]
--
```

### Friends and Followers

#### List Athlete Friends

``` hs
let page = Just 1
let perPage = Just 200
getCurrentFriends client page perPage
-- Right [AthleteSummary {..},..]
```

#### List Athlete Followers

``` hs
let page = Just 1
let perPage = Just 200
getCurrentFollowers client page perPage
-- Right [AthleteSummary {..},..]
```

#### List Both Following

``` hs
let athleteId = 65516
let page = Just 1
let perPage = Just 200
getCommonFriends client athleteId page perPage
-- Right [AthleteSummary {..},..]
```

### Activities

#### Create an Activity

<https://github.com/tfausak/strive/issues/12>

#### Retrieve an Activity

``` hs
let activityId = 141273622
let includeAllEfforts = Just True
getActivity client activityId includeAllEfforts
-- Right (ActivitySummary {..})
```

#### Update an Activity

<https://github.com/tfausak/strive/issues/14>

#### Delete an Activity

<https://github.com/tfausak/strive/issues/15>

#### List Athlete Activities

``` hs
import Data.Time.Clock (getCurrentTime)
time <- getCurrentTime
let before = Just time
let after = Nothing
let page = Just 1
let perPage = Just 200
getCurrentActivities client before after page perPage
-- Right [ActivitySummary {..},..]
```

#### List Friends' Activities

``` hs
let page = Just 1
let perPage = Just 200
getFeed client page perPage
-- Right [ActivitySummary {..},..]
```

#### List Activity Zones

``` hs
let activityId = 141273622
getActivityZones client activityId
-- Right [ZoneSummary {..},..]
```

#### List Activity Laps

``` hs
let activityId = 141273622
getActivityLaps client activityId
-- Right [ZoneSummary {..},..]
```

### Comments

#### List Activity Comments

``` hs
let activityId = 42001703
let includeMarkdown = Just False
let page = Just 1
let perPage = Just 200
getActivityComments client activityId includeMarkdown page perPage
-- Right [CommentSummary {..},..]
```

### Kudos

#### List Activity Kudoers

``` hs
let activityId = 141273622
let page = Just 1
let perPage = Just 200
getActivityKudoers client activityId page perPage
-- Right [AthleteSummary {..},..]
```

### Photos

#### List Activity Photos

``` hs
let activityId = 141273622
getActivityPhotos client activityId
-- Right [PhotoSummary {..},..]
```

### Clubs

#### Retrieve a Club

``` hs
let clubId = 11193
getClub client clubId
-- Right (ClubDetailed {..})
```

#### List Athlete Clubs

``` hs
getCurrentClubs client
-- Right [ClubSummary {..},..]
```

#### List Club Members

``` hs
let clubId = 11193
let page = Just 1
let perPage = Just 200
getClubMembers client clubId page perPage
-- Right [AthleteSummary {..},..]
```

#### List Club Activities

``` hs
let clubId = 11193
let page = Just 1
let perPage = Just 200
getClubActivities client clubId page perPage
-- Right [ActivitySummary {..},..]
```

### Gear

#### Retrieve Gear

``` hs
let gearId = "b387855"
getGear client gearId
-- Right (GearDetailed {..})
```

### Segments

#### Retrieve a Segment

``` hs
let segmentId = 4773104
getSegment client segmentId
-- Right (SegmentDetailed {..})
```

#### List Starred Segments

``` hs
let page = Just 1
let perPage = Just 200
getStarredSegments client page perPage
-- Right [SegmentSummary {..},..]
```

#### List Efforts

``` hs
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (UTCTime), getCurrentTime)
time <- getCurrentTime
let after = UTCTime (fromGregorian 1970 0 0) 0
let before = time

let segmentId = 4773104
let range = Just (after, before)
let page = Just 1
let perPage = Just 200
getSegmentEfforts client segmentId range page perPage
-- Right [EffortSummary {..},..]
```

#### Segment Leaderboard

``` hs
let segmentId = 1091029
let gender = Nothing
let ageGroup = Just "0_24"
let weightClass = Just "55_64"
let following = Just False
let clubId = Nothing
let range = Just "this_year"
let page = Just 1
let perPage = Just 200
getSegmentLeaderboard client segmentId gender ageGroup weightClass following clubId range page perPage
-- Right [SegmentLeader {..},..]
```

#### Segment Explorer

``` hs
let south = 32.0
let west = -96.0
let north = 33.0
let east = -95.0
let activityType = Just "riding"
let minCat = Just 0
let maxCat = Just 5
exploreSegments client (south, west, north, east) activityType minCat maxCat
-- Right [SegmentExploration {..},..]
```

### Segment Efforts

#### Retrieve a Segment Effort

``` hs
let effortId = 1595370098
getEffort client effortId
-- Right (EffortSummary {..})
```

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
