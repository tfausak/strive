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
-- TODO
```

#### Retrieve Another Athlete

``` hs
-- TODO
```

#### Update Current Athlete

<https://github.com/tfausak/strive/issues/7>

#### List Athlete K/QOMs/CRs

``` hs
-- TODO
```

### Friends and Follower

#### List Athlete Friends

``` hs
-- TODO
```

#### List Athlete Followers

``` hs
-- TODO
```

#### List Both Following

``` hs
-- TODO
```

### Activities

#### Create an Activity

<https://github.com/tfausak/strive/issues/12>

#### Retrieve an Activity

``` hs
-- TODO
```

#### Update an Activity

<https://github.com/tfausak/strive/issues/14>

#### Delete an Activity

<https://github.com/tfausak/strive/issues/15>

#### List Athlete Activities

``` hs
-- TODO
```

#### List Friends' Activities

``` hs
-- TODO
```

#### List Activity Zones

``` hs
-- TODO
```

#### List Activity Laps

``` hs
-- TODO
```

### Comments

#### List Activity Comments

``` hs
-- TODO
```

### Kudos

#### List Activity Kudoers

``` hs
-- TODO
```

### Photos

#### List Activity Photos

``` hs
-- TODO
```

### Clubs

#### Retrieve a Club

``` hs
-- TODO
```

#### List Athlete Clubs

``` hs
-- TODO
```

#### List Club Members

``` hs
-- TODO
```

#### List Club Activities

``` hs
-- TODO
```

### Gear

#### Retrieve Gear

``` hs
-- TODO
```

### Segments

#### Retrieve a Segment

``` hs
-- TODO
```

#### List Starred Segments

``` hs
-- TODO
```

#### List Efforts

``` hs
-- TODO
```

#### Segment Leaderboard

``` hs
-- TODO
```

#### Segment Explorer

``` hs
-- TODO
```

### Segment Efforts

#### Retrieve a Segment Effort

``` hs
-- TODO
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
