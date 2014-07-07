# Changelog

-   Rewrote everything from the ground up.
-   Prefixed all fields with type names. For example,
    `Strive.Types.Athletes.AthleteMeta.id` became
    `Strive.Types.Athletes.athleteMeta_id`.
-   Reduced the number of modules. For example,
    `Strive.Objects.Athletes.AthleteMeta.AthleteMeta` became
    `Strive.Types.AthleteMeta`.
-   Renamed some types:
    -   `BucketSummary` to `ActivityZoneDistributionBucket`
    -   `EffortLap` to `ActivityLapSummary`
    -   `EffortSummary` to `EffortDetailed`
    -   `SegmentExploration` to `SegmentExplorerEntry`
    -   `SegmentLeader` to `SegmentLeaderboardEntry`
    -   `UploadDetailed` to `UploadStatus`
    -   `ZoneSummary` to `ActivityZoneDetailed`
-   Created some types:
    -   `SegmentExplorerResponse`
    -   `SegmentLeaderboardResponse`
-   Created `Strive.Lenses`, including classes and lenses for all objects.

## v0.4.0 (2014-06-28)

-   Added POST, PUT, and DELETE endpoints.

## v0.3.0 (2014-06-28)

-   Added authorization endpoints.

## v0.2.0 (2014-06-27)

-   Added support for decoding polylines.
-   Added stream endpoints.

## v0.1.1 (2014-06-26)

-   Grouped actions into modules.
-   Moved `Strive.Actions.Internal` to `Strive.Utilities`.
-   Changed `IncludeMarkdown` from `Bool` to `Maybe Bool`.
-   Renamed a bunch of functions:
    -   `getComments` to `getActivityComments`
    -   `getEfforts` to `getSegmentEfforts`
    -   `getFriendsActivities` to `getFeed`
    -   `getKudoers` to `getActivityKudoers`
    -   `getLaps` to `getActivityLaps`
    -   `getLeaders` to `getSegmentLeaderboard`
    -   `getPhotos` to `getActivityPhotos`
    -   `getSegments` to `exploreSegments`
    -   `getZones` to `getActivityZones`
-   Added `athleteId` parameter to `getSegmentEfforts`.

## v0.1.0 (2014-06-24)

-   Initial release.
