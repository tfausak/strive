# Strive

[![CI](https://github.com/tfausak/strive/actions/workflows/ci.yml/badge.svg)](https://github.com/tfausak/strive/actions/workflows/ci.yml)
[![Hackage](https://badgen.net/hackage/v/strive)](https://hackage.haskell.org/package/strive)

Strive is a Haskell client for the [Strava V3 API][].

---

- [Installation](#installation)
- [Usage](#usage)
  - [Authentication](#authentication)
    - [Request access](#request-access)
    - [Token exchange](#token-exchange)
    - [Deauthorization](#deauthorization)
  - [Athletes](#athletes)
    - [Retrieve current athlete](#retrieve-current-athlete)
    - [Retrieve another athlete](#retrieve-another-athlete)
    - [Update current athlete](#update-current-athlete)
    - [Totals and stats](#totals-and-stats)
    - [List athlete K/QOMs/CRs](#list-athlete-kqomscrs)
  - [Friends and followers](#friends-and-followers)
    - [List athlete friends](#list-athlete-friends)
    - [List athlete followers](#list-athlete-followers)
    - [List both following](#list-both-following)
  - [Activities](#activities)
    - [Create an activity](#create-an-activity)
    - [Retrieve an activity](#retrieve-an-activity)
    - [Update an activity](#update-an-activity)
    - [Delete an activity](#delete-an-activity)
    - [List athlete activities](#list-athlete-activities)
    - [List related activities](#list-related-activities)
    - [List friends' activities](#list-friends-activities)
    - [List activity zones](#list-activity-zones)
    - [List activity laps](#list-activity-laps)
  - [Comments](#comments)
    - [List activity comments](#list-activity-comments)
  - [Kudos](#kudos)
    - [List activity kudoers](#list-activity-kudoers)
  - [Photos](#photos)
    - [List activity photos](#list-activity-photos)
  - [Clubs](#clubs)
    - [Retrieve a club](#retrieve-a-club)
    - [List athlete clubs](#list-athlete-clubs)
    - [List club members](#list-club-members)
    - [List club activities](#list-club-activities)
    - [Join a club](#join-a-club)
    - [Leave a club](#leave-a-club)
  - [Gear](#gear)
    - [Retrieve gear](#retrieve-gear)
  - [Segments](#segments)
    - [Retrieve a segment](#retrieve-a-segment)
    - [List starred segments](#list-starred-segments)
    - [List efforts](#list-efforts)
    - [Segment leaderboard](#segment-leaderboard)
    - [Segment explorer](#segment-explorer)
  - [Segment efforts](#segment-efforts)
    - [Retrieve a segment effort](#retrieve-a-segment-effort)
  - [Streams](#streams)
    - [Retrieve activity streams](#retrieve-activity-streams)
    - [Retrieve effort streams](#retrieve-effort-streams)
    - [Retrieve segment streams](#retrieve-segment-streams)
  - [Uploads](#uploads)
    - [Upload an activity](#upload-an-activity)
    - [Check upload status](#check-upload-status)

## Installation

Add it to your Cabal file:

```
build-depends:
  strive
```

Or install it manually:

``` sh
$ cabal update
$ cabal install strive
```

## Usage

To use the API, you'll need an access token. Once you have that, create a new
client using the default HTTP manager.

``` haskell
{-# LANGUAGE OverloadedStrings #-}
import Strive
let token = "..."
client <- buildClient (Just token)
```

Most types implement lenses for their fields. Lenses are preferred over directly
accessing the fields. For instance, instead of doing this:

``` haskell
client_accessToken (client { client_accessToken = "record token" })
-- "record token"
```

Do this:

``` haskell
get accessToken (set accessToken "lens token" client)
-- "lens token"
```

### [Authentication](http://strava.github.io/api/v3/oauth/)

#### [Request access](http://strava.github.io/api/v3/oauth/#get-authorize)

``` haskell
  let authorizeUrl = buildAuthorizeUrl 1790 "http://localhost" $ with
        [ set approvalPrompt False
        , set privateScope True
        , set writeScope True
        , set state "..."
        ]
  print (authorizeUrl :: String)
```

#### [Token exchange](http://strava.github.io/api/v3/oauth/#post-token)

``` haskell
  tokenExchangeResponse <- exchangeToken 1790 "secret" "code"
  print (tokenExchangeResponse :: Result TokenExchangeResponse)
```

#### [Deauthorization](http://strava.github.io/api/v3/oauth/#deauthorize)

``` haskell
  deauthorizationResponse <- deauthorize client
  print (deauthorizationResponse :: Result DeauthorizationResponse)
```

### [Athletes](http://strava.github.io/api/v3/athlete/)

#### [Retrieve current athlete](http://strava.github.io/api/v3/athlete/#get-details)

``` haskell
  currentAthlete <- getCurrentAthlete client
  print (currentAthlete :: Result AthleteDetailed)
```

#### [Retrieve another athlete](http://strava.github.io/api/v3/athlete/#get-another-details)

``` haskell
  anotherAthlete <- getAthlete client 65516
  print (anotherAthlete :: Result AthleteSummary)
```

#### [Update current athlete](http://strava.github.io/api/v3/athlete/#update)

``` haskell
  updatedAthlete <- updateCurrentAthlete client $ with
    [ set city (Just "Dallas")
    , set state (Just "Texas")
    , set country (Just "United States")
    , set sex (Just Male)
    , set weight (Just 72.57)
    ]
  print (updatedAthlete :: Result AthleteDetailed)
```

#### [Totals and stats](http://strava.github.io/api/v3/athlete/#stats)

``` haskell
  athleteStats <- getAthleteStats client 65516
  print (athleteStats :: Result AthleteStats)
```

#### [List athlete K/QOMs/CRs](http://strava.github.io/api/v3/athlete/#koms)

``` haskell
  athleteCrs <- getAthleteCrs client 65516 $ with
    [ set page 1
    , set perPage 2
    ]
  print (athleteCrs :: Result [EffortDetailed])
```

### [Friends and followers](http://strava.github.io/api/v3/follow/)

#### [List athlete friends](http://strava.github.io/api/v3/follow/#friends)

``` haskell
  currentFriends <- getCurrentFriends client $ with
    [ set page 1
    , set perPage 2
    ]
  print (currentFriends :: Result [AthleteSummary])
```

``` haskell
  friends <- getFriends client 65516 $ with
    [ set page 1
    , set perPage 2
    ]
  print (friends :: Result [AthleteSummary])
```

#### [List athlete followers](http://strava.github.io/api/v3/follow/#followers)

``` haskell
  currentFollowers <- getCurrentFollowers client $ with
    [ set page 1
    , set perPage 2
    ]
  print (currentFollowers :: Result [AthleteSummary])
```

``` haskell
  followers <- getFollowers client 65516 $ with
    [ set page 1
    , set perPage 2
    ]
  print (followers :: Result [AthleteSummary])
```

#### [List both following](http://strava.github.io/api/v3/follow/#both)

``` haskell
  commonFriends <- getCommonFriends client 65516 $ with
    [ set page 1
    , set perPage 2
    ]
  print (commonFriends :: Result [AthleteSummary])
```

### [Activities](http://strava.github.io/api/v3/activities/)

#### [Create an activity](http://strava.github.io/api/v3/activities/#create)

``` haskell
  createdActivity <- createActivity client "An Example" Run (UTCTime (fromGregorian 1970 0 0) 0) 10 $ with
    [ set description (Just "...")
    , set distance (Just 100.0)
    ]
  print (createdActivity :: Result ActivityDetailed)
```

#### [Retrieve an activity](http://strava.github.io/api/v3/activities/#get-details)

``` haskell
  activity <- getActivity client 141273622 $ with
    [ set allEfforts True
    ]
  print (activity :: Result ActivitySummary)
```

#### [Update an activity](http://strava.github.io/api/v3/activities/#put-updates)

``` haskell
  updatedActivity <- updateActivity client 141273622 $ with
    [ set name (Just "WedEx Pit Stop")
    , set type_ (Just Ride)
    , set private (Just False)
    , set commute (Just True)
    , set trainer (Just False)
    , set gearId (Just "b387882")
    , set description Nothing
    ]
  print (updatedActivity :: Result ActivityDetailed)
```

#### [Delete an activity](http://strava.github.io/api/v3/activities/#delete)

``` haskell
  deletedActivity <- deleteActivity client 162674281
  print (deletedActivity :: Result ())
```

#### [List athlete activities](http://strava.github.io/api/v3/activities/#get-activities)

``` haskell
  currentActivities <- getCurrentActivities client $ with
    [ set before (Just (UTCTime (fromGregorian 1970 0 0) 0))
    , set after (Just (UTCTime (fromGregorian 1970 0 0) 0))
    , set page 1
    , set perPage 2
    ]
  print (currentActivities :: Result [ActivitySummary])
```

#### [List related activities](http://strava.github.io/api/v3/activities/#get-related)

``` haskell
  relatedActivities <- getRelatedActivities client 141273622 $ with
    [ set page 1
    , set perPage 2
    ]
  print (relatedActivities :: Result [ActivitySummary])
```

#### [List friends' activities](http://strava.github.io/api/v3/activities/#get-feed)

``` haskell
  feed <- getFeed client $ with
    [ set page 1
    , set perPage 2
    ]
  print (feed :: Result [ActivitySummary])
```

#### [List activity zones](http://strava.github.io/api/v3/activities/#zones)

``` haskell
  activityZones <- getActivityZones client 141273622
  print (activityZones :: Result [ActivityZoneDetailed])
```

#### [List activity laps](http://strava.github.io/api/v3/activities/#laps)

``` haskell
  activityLaps <- getActivityLaps client 141273622
  print (activityLaps :: Result [ActivityLapSummary])
```

### [Comments](http://strava.github.io/api/v3/comments/)

#### [List activity comments](http://strava.github.io/api/v3/comments/#list)

``` haskell
  activityComments <- getActivityComments client 90112360 $ with
    [ set markdown True
    , set page 1
    , set perPage 2
    ]
  print (activityComments :: Result [CommentSummary])
```

### [Kudos](http://strava.github.io/api/v3/kudos/)

#### [List activity kudoers](http://strava.github.io/api/v3/kudos/#list)

``` haskell
  activityKudoers <- getActivityKudoers client 141273622 $ with
    [ set page 1
    , set perPage 2
    ]
  print (activityKudoers :: Result [AthleteSummary])
```

### [Photos](http://strava.github.io/api/v3/photos/)

#### [List activity photos](http://strava.github.io/api/v3/photos/#list)

``` haskell
  activityPhotos <- getActivityPhotos client 141273622
  print (activityPhotos :: Result [PhotoSummary])
```

### [Clubs](http://strava.github.io/api/v3/clubs/)

#### [Retrieve a club](http://strava.github.io/api/v3/clubs/#get-details)

``` haskell
  club <- getClub client 11193
  print (club :: Result ClubDetailed)
```

#### [List athlete clubs](http://strava.github.io/api/v3/clubs/#get-athletes)

``` haskell
  currentClubs <- getCurrentClubs client
  print (currentClubs :: Result [ClubSummary])
```

#### [List club members](http://strava.github.io/api/v3/clubs/#get-members)

``` haskell
  clubMembers <- getClubMembers client 11193 $ with
    [ set page 1
    , set perPage 2
    ]
  print (clubMembers :: Result [AthleteSummary])
```

#### [List club activities](http://strava.github.io/api/v3/clubs/#get-activities)

``` haskell
  clubActivities <- getClubActivities client 11193 $ with
    [ set page 1
    , set perPage 2
    ]
  print (clubActivities :: Result [ActivitySummary])
```

#### [Join a club](http://strava.github.io/api/v3/clubs/#join)

``` haskell
  joinedClub <- joinClub client 165
  print (joinedClub :: Result ())
```

#### [Leave a club](http://strava.github.io/api/v3/clubs/#leave)

``` haskell
  leftClub <- leaveClub client 165
  print (leftClub :: Result ())
```

### [Gear](http://strava.github.io/api/v3/gear/)

#### [Retrieve gear](http://strava.github.io/api/v3/gear/#show)

``` haskell
  theGear <- getGear client "b387855"
  print (theGear :: Result GearDetailed)
```

### [Segments](http://strava.github.io/api/v3/segments/)

#### [Retrieve a segment](http://strava.github.io/api/v3/segments/#retrieve)

``` haskell
  theSegment <- getSegment client 4773104
  print (theSegment :: Result SegmentDetailed)
```

#### [List starred segments](http://strava.github.io/api/v3/segments/#starred)

``` haskell
  starredSegments <- getStarredSegments client $ with
    [ set page 1
    , set perPage 2
    ]
  print (starredSegments :: Result [SegmentSummary])
```

#### [List efforts](http://strava.github.io/api/v3/segments/#efforts)

``` haskell
  theSegmentEfforts <- getSegmentEfforts client 4773104 $ with
    [ set athleteId (Just 65516)
    , set range (Just ((UTCTime (fromGregorian 1970 0 0) 0), (UTCTime (fromGregorian 1970 0 0) 0)))
    , set page 1
    , set perPage 2
    ]
  print (theSegmentEfforts :: Result [EffortDetailed])
```

#### [Segment leaderboard](http://strava.github.io/api/v3/segments/#leaderboard)

``` haskell
  segmentLeaderboardResponse <- getSegmentLeaderboard client 4773104 $ with
    [ set gender (Just Male)
    , set ageGroup (Just Ages0To24)
    , set weightClass (Just Kilograms65To74)
    , set following (Just True)
    , set clubId (Just 11193)
    , set dateRange (Just "this_year")
    , set contextEntries (Just 15)
    , set page 1
    , set perPage 2
    ]
  print (segmentLeaderboardResponse :: Result SegmentLeaderboardResponse)
```

#### [Segment explorer](http://strava.github.io/api/v3/segments/#explore)

``` haskell
  segmentExplorerResponse <- exploreSegments client (32.0, -96.0, 33.0, -95.0) $ with
    [ set activityType Riding
    , set minCat 0
    , set maxCat 5
    ]
  print (segmentExplorerResponse :: Result SegmentExplorerResponse)
```

### [Segment efforts](http://strava.github.io/api/v3/efforts/)

#### [Retrieve a segment effort](http://strava.github.io/api/v3/efforts/#retrieve)

``` haskell
  segmentEffort <- getSegmentEffort client 1595370098
  print (segmentEffort :: Result EffortDetailed)
```

### [Streams](http://strava.github.io/api/v3/streams/)

#### [Retrieve activity streams](http://strava.github.io/api/v3/streams/#activity)

``` haskell
  activityStreams <- getActivityStreams client 141273622 [LatlngStream, WattsStream] $ with
    [ set resolution (Just Low)
    , set seriesType Time
    ]
  print (activityStreams :: Result [StreamDetailed])
```

#### [Retrieve effort streams](http://strava.github.io/api/v3/streams/#effort)

``` haskell
  effortStreams <- getEffortStreams client 1595370098 [LatlngStream, WattsStream] $ with
    [ set resolution (Just Low)
    , set seriesType Time
    ]
  print (effortStreams :: Result [StreamDetailed])
```

#### [Retrieve segment streams](http://strava.github.io/api/v3/streams/#segment)

``` haskell
  segmentStreams <- getSegmentStreams client 4773104 [LatlngStream, WattsStream] $ with
    [ set resolution (Just Low)
    , set seriesType Time
    ]
  print (segmentStreams :: Result [StreamDetailed])
```

### [Uploads](http://strava.github.io/api/v3/uploads/)

#### [Upload an activity](http://strava.github.io/api/v3/uploads/#post-file)

``` haskell
  uploadedActivity <- uploadActivity client (pack "...") "gpx.gz" $ with
    [ set activityType (Just Ride)
    , set name (Just "An Example")
    , set description (Just "...")
    , set private True
    , set trainer False
    , set externalId (Just "...")
    ]
  print (uploadedActivity :: Result UploadStatus)
```

#### [Check upload status](http://strava.github.io/api/v3/uploads/#get-status)

``` haskell
  upload <- getUpload client 16486788
  print (upload :: Result UploadStatus)
```

[Strava V3 API]: http://strava.github.io/api/
[Semantic Versioning]: http://semver.org/spec/v2.0.0.html
[the change log]: CHANGELOG.md
