# [Strive][1]

[![Build Status](https://travis-ci.org/tfausak/strive.svg?branch=master)](https://travis-ci.org/tfausak/strive)

A Haskell client for the [Strava V3 API][2].

- [Usage](#usage)
  - [Authentication](#authentication)
    - [Request access](#request-access)
    - [Token exchange](#token-exchange)
    - [Deauthorization](#deauthorization)
  - [Athletes](#athletes)
    - [Retrieve current athlete](#retrieve-current-athlete)
    - [Retrieve another athlete](#retrieve-another-athlete)
    - [Update current athlete](#update-current-athlete)
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

## Usage

To use the API, you'll need an access token. Once you have that, create a new
client using the default HTTP manager.

~~~ {.haskell .ignore}
import Strive
let token = "..."
client <- buildClient token
~~~

Most types implement lenses for their fields. Lenses are preferred over directly
accessing the fields. For instance, instead of doing this:

~~~ {.haskell .ignore}
client_accessToken (client { client_accessToken = "..." })
-- "..."
~~~

Do this:

~~~ {.haskell .ignore}
get accessToken (set accessToken "..." client)
-- "..."
~~~

<!--
~~~ {.haskell}
import Strive
import System.Exit (exitSuccess)

main :: IO ()
main = do
  _ <- exitSuccess
  client <- buildClient "token"
~~~
-->

### [Authentication](http://strava.github.io/api/v3/oauth/)

#### [Request access](http://strava.github.io/api/v3/oauth/#get-authorize)

~~~ {.haskell}
  let authorizeUrl = buildAuthorizeUrl 1790 "http://localhost" $ with
        [ set approvalPrompt True
        , set privateScope True
        , set writeScope True
        , set state "example"
        ]
  print (authorizeUrl :: String)
~~~

#### [Token exchange](http://strava.github.io/api/v3/oauth/#post-token)

~~~ {.haskell}
  tokenExchangeResponse <- exchangeToken 1790 "secret" "code"
  print (tokenExchangeResponse :: Either String TokenExchangeResponse)
~~~

#### [Deauthorization](http://strava.github.io/api/v3/oauth/#deauthorize)

~~~ {.haskell}
  deauthorizationResponse <- deauthorize client
  print (deauthorizationResponse :: Either String DeauthorizationResponse)
~~~

### [Athletes](http://strava.github.io/api/v3/athlete/)

#### [Retrieve current athlete](http://strava.github.io/api/v3/athlete/#get-details)

~~~ {.haskell}
  currentAthlete <- getCurrentAthlete client
  print (currentAthlete :: Either String AthleteDetailed)
~~~

#### [Retrieve another athlete](http://strava.github.io/api/v3/athlete/#get-another-details)

~~~ {.haskell}
  anotherAthlete <- getAthlete client 65516
  print (anotherAthlete :: Either String AthleteSummary)
~~~

#### [Update current athlete](http://strava.github.io/api/v3/athlete/#update)

~~~ {.haskell}
  updatedAthlete <- updateCurrentAthlete client $ with
    [ set city (Just "Dallas")
    , set state (Just "TX")
    , set country (Just "United States")
    , set sex (Just 'M')
    , set weight (Just 72.57)
    ]
  print (updatedAthlete :: Either String AthleteDetailed)
~~~

#### [List athlete K/QOMs/CRs](http://strava.github.io/api/v3/athlete/#koms)

~~~ {.haskell}
  athleteCrs <- getAthleteCrs client 65516 $ with
    [ set page 1
    , set perPage 2
    ]
  print (athleteCrs :: Either String [EffortDetailed])
~~~

### [Friends and followers](http://strava.github.io/api/v3/follow/)

#### [List athlete friends](http://strava.github.io/api/v3/follow/#friends)

~~~ {.haskell}
  currentFriends <- getCurrentFriends client $ with
    [ set page 1
    , set perPage 2
    ]
  print (currentFriends :: Either String [AthleteSummary])
~~~

#### [List athlete followers](http://strava.github.io/api/v3/follow/#followers)

~~~ {.haskell}
~~~

#### [List both following](http://strava.github.io/api/v3/follow/#both)

~~~ {.haskell}
~~~

### [Activities](http://strava.github.io/api/v3/activities/)

#### [Create an activity](http://strava.github.io/api/v3/activities/#create)

~~~ {.haskell}
~~~

#### [Retrieve an activity](http://strava.github.io/api/v3/activities/#get-details)

~~~ {.haskell}
~~~

#### [Update an activity](http://strava.github.io/api/v3/activities/#put-updates)

~~~ {.haskell}
~~~

#### [Delete an activity](http://strava.github.io/api/v3/activities/#delete)

~~~ {.haskell}
~~~

#### [List athlete activities](http://strava.github.io/api/v3/activities/#get-activities)

~~~ {.haskell}
~~~

#### [List friends' activities](http://strava.github.io/api/v3/activities/#get-feed)

~~~ {.haskell}
~~~

#### [List activity zones](http://strava.github.io/api/v3/activities/#zones)

~~~ {.haskell}
~~~

#### [List activity laps](http://strava.github.io/api/v3/activities/#laps)

~~~ {.haskell}
~~~

### [Comments](http://strava.github.io/api/v3/comments/)

#### [List activity comments](http://strava.github.io/api/v3/comments/#list)

~~~ {.haskell}
~~~

### [Kudos](http://strava.github.io/api/v3/kudos/)

#### [List activity kudoers](http://strava.github.io/api/v3/kudos/#list)

~~~ {.haskell}
~~~

### [Photos](http://strava.github.io/api/v3/photos/)

#### [List activity photos](http://strava.github.io/api/v3/photos/#list)

~~~ {.haskell}
~~~

### [Clubs](http://strava.github.io/api/v3/clubs/)

#### [Retrieve a club](http://strava.github.io/api/v3/clubs/#get-details)

~~~ {.haskell}
~~~

#### [List athlete clubs](http://strava.github.io/api/v3/clubs/#get-athletes)

~~~ {.haskell}
~~~

#### [List club members](http://strava.github.io/api/v3/clubs/#get-members)

~~~ {.haskell}
~~~

#### [List club activities](http://strava.github.io/api/v3/clubs/#get-activities)

~~~ {.haskell}
~~~

### [Gear](http://strava.github.io/api/v3/gear/)

#### [Retrieve gear](http://strava.github.io/api/v3/gear/#show)

~~~ {.haskell}
~~~

### [Segments](http://strava.github.io/api/v3/segments/)

#### [Retrieve a segment](http://strava.github.io/api/v3/segments/#retrieve)

~~~ {.haskell}
~~~

#### [List starred segments](http://strava.github.io/api/v3/segments/#starred)

~~~ {.haskell}
~~~

#### [List efforts](http://strava.github.io/api/v3/segments/#efforts)

~~~ {.haskell}
~~~

#### [Segment leaderboard](http://strava.github.io/api/v3/segments/#leaderboard)

~~~ {.haskell}
~~~

#### [Segment explorer](http://strava.github.io/api/v3/segments/#explore)

~~~ {.haskell}
~~~

### [Segment efforts](http://strava.github.io/api/v3/efforts/)

#### [Retrieve a segment effort](http://strava.github.io/api/v3/efforts/#retrieve)

~~~ {.haskell}
~~~

### [Streams](http://strava.github.io/api/v3/streams/)

#### [Retrieve activity streams](http://strava.github.io/api/v3/streams/#activity)

~~~ {.haskell}
~~~

#### [Retrieve effort streams](http://strava.github.io/api/v3/streams/#effort)

~~~ {.haskell}
~~~

#### [Retrieve segment streams](http://strava.github.io/api/v3/streams/#segment)

~~~ {.haskell}
~~~

### [Uploads](http://strava.github.io/api/v3/uploads/)

#### [Upload an activity](http://strava.github.io/api/v3/uploads/#post-file)

~~~ {.haskell}
~~~

#### [Check upload status](http://strava.github.io/api/v3/uploads/#get-status)

~~~ {.haskell}
~~~

<!--
~~~ {.haskell}
  return ()
~~~
-->

[1]: https://github.com/tfausak/strive
[2]: http://strava.github.io/api/
