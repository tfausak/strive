# Changelog

This package uses [Semantic Versioning][1].

## v1.0.0 (2015-04-05)

- Added the response to the error message when decoding fails.
- Changed the input of `buildClient` from a `String` to a `Maybe Text`.
- Added weight to detailed athlete representation.
- Created type aliases for action inputs.

## v0.8.0 (2015-03-02)

- Fixed `FromJSON` instances of polylines.
- Added `getAthleteStats` for getting athlete stats.
- Added `contextEntries` option to `getSegmentLeaderboard`.
- Fixed capitalization of activity types. For example, `Alpineski` is now
  `AlpineSki`.

## v0.7.1 (2015-01-18)

- Fixed `Setup.hs`.

## v0.7.0 (2015-01-17)

- Added `getRelatedActivities` for getting related activities.
- Added `leaveClub` for leaving a club.
- Added `joinClub` for joining a club.
- Added `weighted_average_watts` to activities.
- Added `device_watts` to activities.
- Added some new activity types.

## v0.6.2 (2015-01-17)

- Fixed parsing of activity types.
- Exposed `Strive.Internal` modules.

## v0.6.1 (2014-08-12)

- Moved `Strive.Actions.with` to `Strive.Utilities.with`.
- Created `Strive.Enums.StreamType`.

## v0.6.0 (2014-07-30)

- Derived lens classes and instances using template Haskell.
- Went back to clobbering Prelude exports (and keywords).
- Added "Response" to `SegmentLeaderboardResponse` field names.
- Created Vagrant configuration.
- Derived JSON instances using template Haskell.
- Replaced `client_httpManager` with `client_requester`.
- Removed `?` infix operator.
- Switched to functor-based lenses.
- Created enums for some options.
- Added support for GHC 7.8.2.

## v0.5.1 (2014-07-08)

- Avoided clobbering Prelude exports by appending underscores (`id` became
  `id_`).
- Created infix operator `?` as a shortcut for `$ with`.
- Changed return of `deleteActivity` from `Value` to `()`.
- Created `Show` instance for `Client`.
- Fixed `README.lhs` on Windows.

## v0.5.0 (2014-07-07)

- Rewrote everything from the ground up.
- Prefixed all fields with type names. For example,
  `Strive.Types.Athletes.AthleteMeta.id` became
  `Strive.Types.Athletes.athleteMeta_id`.
- Reduced the number of modules. For example,
  `Strive.Objects.Athletes.AthleteMeta.AthleteMeta` became
  `Strive.Types.AthleteMeta`.
- Renamed some types:
  - `BucketSummary` to `ActivityZoneDistributionBucket`
  - `EffortLap` to `ActivityLapSummary`
  - `EffortSummary` to `EffortDetailed`
  - `SegmentExploration` to `SegmentExplorerEntry`
  - `SegmentLeader` to `SegmentLeaderboardEntry`
  - `UploadDetailed` to `UploadStatus`
  - `ZoneSummary` to `ActivityZoneDetailed`
- Created some types:
  - `SegmentExplorerResponse`
  - `SegmentLeaderboardResponse`
- Created `Strive.Lenses`, including classes and lenses for all objects.

## v0.4.0 (2014-06-28)

- Added POST, PUT, and DELETE endpoints.

## v0.3.0 (2014-06-28)

- Added authorization endpoints.

## v0.2.0 (2014-06-27)

- Added support for decoding polylines.
- Added stream endpoints.

## v0.1.1 (2014-06-26)

- Grouped actions into modules.
- Moved `Strive.Actions.Internal` to `Strive.Utilities`.
- Changed `IncludeMarkdown` from `Bool` to `Maybe Bool`.
- Renamed a bunch of functions:
  - `getComments` to `getActivityComments`
  - `getEfforts` to `getSegmentEfforts`
  - `getFriendsActivities` to `getFeed`
  - `getKudoers` to `getActivityKudoers`
  - `getLaps` to `getActivityLaps`
  - `getLeaders` to `getSegmentLeaderboard`
  - `getPhotos` to `getActivityPhotos`
  - `getSegments` to `exploreSegments`
  - `getZones` to `getActivityZones`
- Added `athleteId` parameter to `getSegmentEfforts`.

## v0.1.0 (2014-06-24)

- Initial release.

[1]: http://semver.org/spec/v2.0.0.html
