# Changelog

## v0.2.0 (2014-06-27)

- Added support for decoding polylines.
- Added stream endpoints.

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
- Added `athleteId` parameter to `getSegmentEfforts`.

## v0.1.0 (2014-06-24)

-   Initial release.
