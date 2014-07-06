{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Strive.Lenses.Instances where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Network.HTTP.Client.Conduit (Manager)
import Strive.Client
import Strive.Lenses.Classes
import Strive.Options
import Strive.Types

instance AccessTokenLens Client String where
  accessToken client =
    ( client_accessToken client
    , \ accessToken' -> client { client_accessToken = accessToken' }
    )

instance AccessTokenLens DeauthorizationResponse Text where
  accessToken deauthorizationResponse =
    ( deauthorizationResponse_accessToken deauthorizationResponse
    , \ accessToken' -> deauthorizationResponse { deauthorizationResponse_accessToken = accessToken' }
    )

instance AccessTokenLens TokenExchangeResponse Text where
  accessToken tokenExchangeResponse =
    ( tokenExchangeResponse_accessToken tokenExchangeResponse
    , \ accessToken' -> tokenExchangeResponse { tokenExchangeResponse_accessToken = accessToken' }
    )

instance AchievementCountLens ActivityDetailed Integer where
  achievementCount activityDetailed =
    ( activityDetailed_achievementCount activityDetailed
    , \ achievementCount' -> activityDetailed { activityDetailed_achievementCount = achievementCount' }
    )

instance AchievementCountLens ActivitySummary Integer where
  achievementCount activitySummary =
    ( activitySummary_achievementCount activitySummary
    , \ achievementCount' -> activitySummary { activitySummary_achievementCount = achievementCount' }
    )

instance ActivityIdLens ActivityLapSummary Integer where
  activityId activityLapSummary =
    ( activityLapSummary_activityId activityLapSummary
    , \ activityId' -> activityLapSummary { activityLapSummary_activityId = activityId' }
    )

instance ActivityIdLens CommentSummary Integer where
  activityId commentSummary =
    ( commentSummary_activityId commentSummary
    , \ activityId' -> commentSummary { commentSummary_activityId = activityId' }
    )

instance ActivityIdLens EffortDetailed Integer where
  activityId effortDetailed =
    ( effortDetailed_activityId effortDetailed
    , \ activityId' -> effortDetailed { effortDetailed_activityId = activityId' }
    )

instance ActivityIdLens PhotoSummary Integer where
  activityId photoSummary =
    ( photoSummary_activityId photoSummary
    , \ activityId' -> photoSummary { photoSummary_activityId = activityId' }
    )

instance ActivityTypeLens SegmentDetailed Text where
  activityType segmentDetailed =
    ( segmentDetailed_activityType segmentDetailed
    , \ activityType' -> segmentDetailed { segmentDetailed_activityType = activityType' }
    )

instance ActivityTypeLens SegmentSummary Text where
  activityType segmentSummary =
    ( segmentSummary_activityType segmentSummary
    , \ activityType' -> segmentSummary { segmentSummary_activityType = activityType' }
    )

instance AfterLens GetCurrentActivitiesOptions (Maybe UTCTime) where
  after getCurrentActivitiesOptions =
    ( getCurrentActivitiesOptions_after getCurrentActivitiesOptions
    , \ after' -> getCurrentActivitiesOptions { getCurrentActivitiesOptions_after = after' }
    )

instance AllEffortsLens GetActivityOptions Bool where
  allEfforts getActivityOptions =
    ( getActivityOptions_allEfforts getActivityOptions
    , \ allEfforts' -> getActivityOptions { getActivityOptions_allEfforts = allEfforts' }
    )

instance ApprovalPromptLens BuildAuthorizeUrlOptions Bool where
  approvalPrompt buildAuthorizeUrlOptions =
    ( buildAuthorizeUrlOptions_approvalPrompt buildAuthorizeUrlOptions
    , \ approvalPrompt' -> buildAuthorizeUrlOptions { buildAuthorizeUrlOptions_approvalPrompt = approvalPrompt' }
    )

instance AthleteCountLens ActivityDetailed Integer where
  athleteCount activityDetailed =
    ( activityDetailed_athleteCount activityDetailed
    , \ athleteCount' -> activityDetailed { activityDetailed_athleteCount = athleteCount' }
    )

instance AthleteCountLens ActivitySummary Integer where
  athleteCount activitySummary =
    ( activitySummary_athleteCount activitySummary
    , \ athleteCount' -> activitySummary { activitySummary_athleteCount = athleteCount' }
    )

instance AthleteCountLens SegmentDetailed Integer where
  athleteCount segmentDetailed =
    ( segmentDetailed_athleteCount segmentDetailed
    , \ athleteCount' -> segmentDetailed { segmentDetailed_athleteCount = athleteCount' }
    )

instance AthleteIdLens ActivityLapSummary Integer where
  athleteId activityLapSummary =
    ( activityLapSummary_athleteId activityLapSummary
    , \ athleteId' -> activityLapSummary { activityLapSummary_athleteId = athleteId' }
    )

instance AthleteIdLens EffortDetailed Integer where
  athleteId effortDetailed =
    ( effortDetailed_athleteId effortDetailed
    , \ athleteId' -> effortDetailed { effortDetailed_athleteId = athleteId' }
    )

instance AthleteLens ActivityDetailed AthleteMeta where
  athlete activityDetailed =
    ( activityDetailed_athlete activityDetailed
    , \ athlete' -> activityDetailed { activityDetailed_athlete = athlete' }
    )

instance AthleteLens ActivitySummary AthleteMeta where
  athlete activitySummary =
    ( activitySummary_athlete activitySummary
    , \ athlete' -> activitySummary { activitySummary_athlete = athlete' }
    )

instance AthleteLens CommentSummary AthleteSummary where
  athlete commentSummary =
    ( commentSummary_athlete commentSummary
    , \ athlete' -> commentSummary { commentSummary_athlete = athlete' }
    )

instance AthleteLens TokenExchangeResponse AthleteDetailed where
  athlete tokenExchangeResponse =
    ( tokenExchangeResponse_athlete tokenExchangeResponse
    , \ athlete' -> tokenExchangeResponse { tokenExchangeResponse_athlete = athlete' }
    )

instance AverageCadenceLens EffortDetailed (Maybe Double) where
  averageCadence effortDetailed =
    ( effortDetailed_averageCadence effortDetailed
    , \ averageCadence' -> effortDetailed { effortDetailed_averageCadence = averageCadence' }
    )

instance AverageGradeLens SegmentDetailed Double where
  averageGrade segmentDetailed =
    ( segmentDetailed_averageGrade segmentDetailed
    , \ averageGrade' -> segmentDetailed { segmentDetailed_averageGrade = averageGrade' }
    )

instance AverageGradeLens SegmentSummary Double where
  averageGrade segmentSummary =
    ( segmentSummary_averageGrade segmentSummary
    , \ averageGrade' -> segmentSummary { segmentSummary_averageGrade = averageGrade' }
    )

instance AverageHeartrateLens EffortDetailed (Maybe Double) where
  averageHeartrate effortDetailed =
    ( effortDetailed_averageHeartrate effortDetailed
    , \ averageHeartrate' -> effortDetailed { effortDetailed_averageHeartrate = averageHeartrate' }
    )

instance AverageSpeedLens ActivityDetailed Double where
  averageSpeed activityDetailed =
    ( activityDetailed_averageSpeed activityDetailed
    , \ averageSpeed' -> activityDetailed { activityDetailed_averageSpeed = averageSpeed' }
    )

instance AverageSpeedLens ActivityLapSummary Double where
  averageSpeed activityLapSummary =
    ( activityLapSummary_averageSpeed activityLapSummary
    , \ averageSpeed' -> activityLapSummary { activityLapSummary_averageSpeed = averageSpeed' }
    )

instance AverageSpeedLens ActivitySummary Double where
  averageSpeed activitySummary =
    ( activitySummary_averageSpeed activitySummary
    , \ averageSpeed' -> activitySummary { activitySummary_averageSpeed = averageSpeed' }
    )

instance AverageWattsLens ActivityDetailed (Maybe Double) where
  averageWatts activityDetailed =
    ( activityDetailed_averageWatts activityDetailed
    , \ averageWatts' -> activityDetailed { activityDetailed_averageWatts = averageWatts' }
    )

instance AverageWattsLens ActivityLapSummary Double where
  averageWatts activityLapSummary =
    ( activityLapSummary_averageWatts activityLapSummary
    , \ averageWatts' -> activityLapSummary { activityLapSummary_averageWatts = averageWatts' }
    )

instance AverageWattsLens ActivitySummary (Maybe Double) where
  averageWatts activitySummary =
    ( activitySummary_averageWatts activitySummary
    , \ averageWatts' -> activitySummary { activitySummary_averageWatts = averageWatts' }
    )

instance AverageWattsLens EffortDetailed (Maybe Double) where
  averageWatts effortDetailed =
    ( effortDetailed_averageWatts effortDetailed
    , \ averageWatts' -> effortDetailed { effortDetailed_averageWatts = averageWatts' }
    )

instance BeforeLens GetCurrentActivitiesOptions (Maybe UTCTime) where
  before getCurrentActivitiesOptions =
    ( getCurrentActivitiesOptions_before getCurrentActivitiesOptions
    , \ before' -> getCurrentActivitiesOptions { getCurrentActivitiesOptions_before = before' }
    )

instance BikesLens AthleteDetailed [GearSummary] where
  bikes athleteDetailed =
    ( athleteDetailed_bikes athleteDetailed
    , \ bikes' -> athleteDetailed { athleteDetailed_bikes = bikes' }
    )

instance BrandNameLens GearDetailed Text where
  brandName gearDetailed =
    ( gearDetailed_brandName gearDetailed
    , \ brandName' -> gearDetailed { gearDetailed_brandName = brandName' }
    )

instance CaloriesLens ActivityDetailed Double where
  calories activityDetailed =
    ( activityDetailed_calories activityDetailed
    , \ calories' -> activityDetailed { activityDetailed_calories = calories' }
    )

instance CaptionLens PhotoSummary Text where
  caption photoSummary =
    ( photoSummary_caption photoSummary
    , \ caption' -> photoSummary { photoSummary_caption = caption' }
    )

instance CityLens AthleteDetailed Text where
  city athleteDetailed =
    ( athleteDetailed_city athleteDetailed
    , \ city' -> athleteDetailed { athleteDetailed_city = city' }
    )

instance CityLens AthleteSummary (Maybe Text) where
  city athleteSummary =
    ( athleteSummary_city athleteSummary
    , \ city' -> athleteSummary { athleteSummary_city = city' }
    )

instance CityLens ClubDetailed Text where
  city clubDetailed =
    ( clubDetailed_city clubDetailed
    , \ city' -> clubDetailed { clubDetailed_city = city' }
    )

instance CityLens SegmentDetailed Text where
  city segmentDetailed =
    ( segmentDetailed_city segmentDetailed
    , \ city' -> segmentDetailed { segmentDetailed_city = city' }
    )

instance CityLens SegmentSummary Text where
  city segmentSummary =
    ( segmentSummary_city segmentSummary
    , \ city' -> segmentSummary { segmentSummary_city = city' }
    )

instance CityLens UpdateCurrentAthleteOptions (Maybe String) where
  city updateCurrentAthleteOptions =
    ( updateCurrentAthleteOptions_city updateCurrentAthleteOptions
    , \ city' -> updateCurrentAthleteOptions { updateCurrentAthleteOptions_city = city' }
    )

instance ClimbCategoryLens SegmentDetailed Integer where
  climbCategory segmentDetailed =
    ( segmentDetailed_climbCategory segmentDetailed
    , \ climbCategory' -> segmentDetailed { segmentDetailed_climbCategory = climbCategory' }
    )

instance ClimbCategoryLens SegmentSummary Integer where
  climbCategory segmentSummary =
    ( segmentSummary_climbCategory segmentSummary
    , \ climbCategory' -> segmentSummary { segmentSummary_climbCategory = climbCategory' }
    )

instance ClubTypeLens ClubDetailed Text where
  clubType clubDetailed =
    ( clubDetailed_clubType clubDetailed
    , \ clubType' -> clubDetailed { clubDetailed_clubType = clubType' }
    )

instance ClubsLens AthleteDetailed [ClubSummary] where
  clubs athleteDetailed =
    ( athleteDetailed_clubs athleteDetailed
    , \ clubs' -> athleteDetailed { athleteDetailed_clubs = clubs' }
    )

instance CommentCountLens ActivityDetailed Integer where
  commentCount activityDetailed =
    ( activityDetailed_commentCount activityDetailed
    , \ commentCount' -> activityDetailed { activityDetailed_commentCount = commentCount' }
    )

instance CommentCountLens ActivitySummary Integer where
  commentCount activitySummary =
    ( activitySummary_commentCount activitySummary
    , \ commentCount' -> activitySummary { activitySummary_commentCount = commentCount' }
    )

instance CommuteLens ActivityDetailed Bool where
  commute activityDetailed =
    ( activityDetailed_commute activityDetailed
    , \ commute' -> activityDetailed { activityDetailed_commute = commute' }
    )

instance CommuteLens ActivitySummary Bool where
  commute activitySummary =
    ( activitySummary_commute activitySummary
    , \ commute' -> activitySummary { activitySummary_commute = commute' }
    )

instance CommuteLens UpdateActivityOptions (Maybe Bool) where
  commute updateActivityOptions =
    ( updateActivityOptions_commute updateActivityOptions
    , \ commute' -> updateActivityOptions { updateActivityOptions_commute = commute' }
    )

instance CountryLens AthleteDetailed Text where
  country athleteDetailed =
    ( athleteDetailed_country athleteDetailed
    , \ country' -> athleteDetailed { athleteDetailed_country = country' }
    )

instance CountryLens AthleteSummary (Maybe Text) where
  country athleteSummary =
    ( athleteSummary_country athleteSummary
    , \ country' -> athleteSummary { athleteSummary_country = country' }
    )

instance CountryLens ClubDetailed Text where
  country clubDetailed =
    ( clubDetailed_country clubDetailed
    , \ country' -> clubDetailed { clubDetailed_country = country' }
    )

instance CountryLens SegmentDetailed Text where
  country segmentDetailed =
    ( segmentDetailed_country segmentDetailed
    , \ country' -> segmentDetailed { segmentDetailed_country = country' }
    )

instance CountryLens SegmentSummary Text where
  country segmentSummary =
    ( segmentSummary_country segmentSummary
    , \ country' -> segmentSummary { segmentSummary_country = country' }
    )

instance CountryLens UpdateCurrentAthleteOptions (Maybe String) where
  country updateCurrentAthleteOptions =
    ( updateCurrentAthleteOptions_country updateCurrentAthleteOptions
    , \ country' -> updateCurrentAthleteOptions { updateCurrentAthleteOptions_country = country' }
    )

instance CreatedAtLens AthleteDetailed UTCTime where
  createdAt athleteDetailed =
    ( athleteDetailed_createdAt athleteDetailed
    , \ createdAt' -> athleteDetailed { athleteDetailed_createdAt = createdAt' }
    )

instance CreatedAtLens AthleteSummary UTCTime where
  createdAt athleteSummary =
    ( athleteSummary_createdAt athleteSummary
    , \ createdAt' -> athleteSummary { athleteSummary_createdAt = createdAt' }
    )

instance CreatedAtLens CommentSummary UTCTime where
  createdAt commentSummary =
    ( commentSummary_createdAt commentSummary
    , \ createdAt' -> commentSummary { commentSummary_createdAt = createdAt' }
    )

instance CreatedAtLens PhotoSummary UTCTime where
  createdAt photoSummary =
    ( photoSummary_createdAt photoSummary
    , \ createdAt' -> photoSummary { photoSummary_createdAt = createdAt' }
    )

instance CreatedAtLens SegmentDetailed UTCTime where
  createdAt segmentDetailed =
    ( segmentDetailed_createdAt segmentDetailed
    , \ createdAt' -> segmentDetailed { segmentDetailed_createdAt = createdAt' }
    )

instance DatePreferenceLens AthleteDetailed Text where
  datePreference athleteDetailed =
    ( athleteDetailed_datePreference athleteDetailed
    , \ datePreference' -> athleteDetailed { athleteDetailed_datePreference = datePreference' }
    )

instance DescriptionLens ActivityDetailed Text where
  description activityDetailed =
    ( activityDetailed_description activityDetailed
    , \ description' -> activityDetailed { activityDetailed_description = description' }
    )

instance DescriptionLens ClubDetailed Text where
  description clubDetailed =
    ( clubDetailed_description clubDetailed
    , \ description' -> clubDetailed { clubDetailed_description = description' }
    )

instance DescriptionLens CreateActivityOptions (Maybe String) where
  description createActivityOptions =
    ( createActivityOptions_description createActivityOptions
    , \ description' -> createActivityOptions { createActivityOptions_description = description' }
    )

instance DescriptionLens GearDetailed Text where
  description gearDetailed =
    ( gearDetailed_description gearDetailed
    , \ description' -> gearDetailed { gearDetailed_description = description' }
    )

instance DescriptionLens UpdateActivityOptions (Maybe String) where
  description updateActivityOptions =
    ( updateActivityOptions_description updateActivityOptions
    , \ description' -> updateActivityOptions { updateActivityOptions_description = description' }
    )

instance DistanceLens ActivityDetailed Double where
  distance activityDetailed =
    ( activityDetailed_distance activityDetailed
    , \ distance' -> activityDetailed { activityDetailed_distance = distance' }
    )

instance DistanceLens ActivityLapSummary Double where
  distance activityLapSummary =
    ( activityLapSummary_distance activityLapSummary
    , \ distance' -> activityLapSummary { activityLapSummary_distance = distance' }
    )

instance DistanceLens ActivitySummary Double where
  distance activitySummary =
    ( activitySummary_distance activitySummary
    , \ distance' -> activitySummary { activitySummary_distance = distance' }
    )

instance DistanceLens CreateActivityOptions (Maybe Double) where
  distance createActivityOptions =
    ( createActivityOptions_distance createActivityOptions
    , \ distance' -> createActivityOptions { createActivityOptions_distance = distance' }
    )

instance DistanceLens EffortDetailed Double where
  distance effortDetailed =
    ( effortDetailed_distance effortDetailed
    , \ distance' -> effortDetailed { effortDetailed_distance = distance' }
    )

instance DistanceLens GearDetailed Double where
  distance gearDetailed =
    ( gearDetailed_distance gearDetailed
    , \ distance' -> gearDetailed { gearDetailed_distance = distance' }
    )

instance DistanceLens GearSummary Double where
  distance gearSummary =
    ( gearSummary_distance gearSummary
    , \ distance' -> gearSummary { gearSummary_distance = distance' }
    )

instance DistanceLens SegmentDetailed Double where
  distance segmentDetailed =
    ( segmentDetailed_distance segmentDetailed
    , \ distance' -> segmentDetailed { segmentDetailed_distance = distance' }
    )

instance DistanceLens SegmentSummary Double where
  distance segmentSummary =
    ( segmentSummary_distance segmentSummary
    , \ distance' -> segmentSummary { segmentSummary_distance = distance' }
    )

instance DistributionBucketsLens ActivityZoneDetailed [ActivityZoneDistributionBucket] where
  distributionBuckets activityZoneDetailed =
    ( activityZoneDetailed_distributionBuckets activityZoneDetailed
    , \ distributionBuckets' -> activityZoneDetailed { activityZoneDetailed_distributionBuckets = distributionBuckets' }
    )

instance EffortCountLens SegmentDetailed Integer where
  effortCount segmentDetailed =
    ( segmentDetailed_effortCount segmentDetailed
    , \ effortCount' -> segmentDetailed { segmentDetailed_effortCount = effortCount' }
    )

instance ElapsedTimeLens ActivityDetailed Integer where
  elapsedTime activityDetailed =
    ( activityDetailed_elapsedTime activityDetailed
    , \ elapsedTime' -> activityDetailed { activityDetailed_elapsedTime = elapsedTime' }
    )

instance ElapsedTimeLens ActivityLapSummary Integer where
  elapsedTime activityLapSummary =
    ( activityLapSummary_elapsedTime activityLapSummary
    , \ elapsedTime' -> activityLapSummary { activityLapSummary_elapsedTime = elapsedTime' }
    )

instance ElapsedTimeLens ActivitySummary Integer where
  elapsedTime activitySummary =
    ( activitySummary_elapsedTime activitySummary
    , \ elapsedTime' -> activitySummary { activitySummary_elapsedTime = elapsedTime' }
    )

instance ElapsedTimeLens EffortDetailed Integer where
  elapsedTime effortDetailed =
    ( effortDetailed_elapsedTime effortDetailed
    , \ elapsedTime' -> effortDetailed { effortDetailed_elapsedTime = elapsedTime' }
    )

instance ElevationHighLens SegmentDetailed Double where
  elevationHigh segmentDetailed =
    ( segmentDetailed_elevationHigh segmentDetailed
    , \ elevationHigh' -> segmentDetailed { segmentDetailed_elevationHigh = elevationHigh' }
    )

instance ElevationHighLens SegmentSummary Double where
  elevationHigh segmentSummary =
    ( segmentSummary_elevationHigh segmentSummary
    , \ elevationHigh' -> segmentSummary { segmentSummary_elevationHigh = elevationHigh' }
    )

instance ElevationLowLens SegmentDetailed Double where
  elevationLow segmentDetailed =
    ( segmentDetailed_elevationLow segmentDetailed
    , \ elevationLow' -> segmentDetailed { segmentDetailed_elevationLow = elevationLow' }
    )

instance ElevationLowLens SegmentSummary Double where
  elevationLow segmentSummary =
    ( segmentSummary_elevationLow segmentSummary
    , \ elevationLow' -> segmentSummary { segmentSummary_elevationLow = elevationLow' }
    )

instance EmailLens AthleteDetailed Text where
  email athleteDetailed =
    ( athleteDetailed_email athleteDetailed
    , \ email' -> athleteDetailed { athleteDetailed_email = email' }
    )

instance EndIndexLens ActivityLapSummary Integer where
  endIndex activityLapSummary =
    ( activityLapSummary_endIndex activityLapSummary
    , \ endIndex' -> activityLapSummary { activityLapSummary_endIndex = endIndex' }
    )

instance EndIndexLens EffortDetailed Integer where
  endIndex effortDetailed =
    ( effortDetailed_endIndex effortDetailed
    , \ endIndex' -> effortDetailed { effortDetailed_endIndex = endIndex' }
    )

instance EndLatitudeLens SegmentDetailed Double where
  endLatitude segmentDetailed =
    ( segmentDetailed_endLatitude segmentDetailed
    , \ endLatitude' -> segmentDetailed { segmentDetailed_endLatitude = endLatitude' }
    )

instance EndLatitudeLens SegmentSummary Double where
  endLatitude segmentSummary =
    ( segmentSummary_endLatitude segmentSummary
    , \ endLatitude' -> segmentSummary { segmentSummary_endLatitude = endLatitude' }
    )

instance EndLatlngLens ActivityDetailed (Maybe (Double, Double)) where
  endLatlng activityDetailed =
    ( activityDetailed_endLatlng activityDetailed
    , \ endLatlng' -> activityDetailed { activityDetailed_endLatlng = endLatlng' }
    )

instance EndLatlngLens ActivitySummary (Maybe (Double, Double)) where
  endLatlng activitySummary =
    ( activitySummary_endLatlng activitySummary
    , \ endLatlng' -> activitySummary { activitySummary_endLatlng = endLatlng' }
    )

instance EndLatlngLens SegmentDetailed ((Double, Double)) where
  endLatlng segmentDetailed =
    ( segmentDetailed_endLatlng segmentDetailed
    , \ endLatlng' -> segmentDetailed { segmentDetailed_endLatlng = endLatlng' }
    )

instance EndLatlngLens SegmentSummary ((Double, Double)) where
  endLatlng segmentSummary =
    ( segmentSummary_endLatlng segmentSummary
    , \ endLatlng' -> segmentSummary { segmentSummary_endLatlng = endLatlng' }
    )

instance EndLongitudeLens SegmentDetailed Double where
  endLongitude segmentDetailed =
    ( segmentDetailed_endLongitude segmentDetailed
    , \ endLongitude' -> segmentDetailed { segmentDetailed_endLongitude = endLongitude' }
    )

instance EndLongitudeLens SegmentSummary Double where
  endLongitude segmentSummary =
    ( segmentSummary_endLongitude segmentSummary
    , \ endLongitude' -> segmentSummary { segmentSummary_endLongitude = endLongitude' }
    )

instance ExternalIdLens ActivityDetailed (Maybe Text) where
  externalId activityDetailed =
    ( activityDetailed_externalId activityDetailed
    , \ externalId' -> activityDetailed { activityDetailed_externalId = externalId' }
    )

instance ExternalIdLens ActivitySummary (Maybe Text) where
  externalId activitySummary =
    ( activitySummary_externalId activitySummary
    , \ externalId' -> activitySummary { activitySummary_externalId = externalId' }
    )

instance FirstnameLens AthleteDetailed Text where
  firstname athleteDetailed =
    ( athleteDetailed_firstname athleteDetailed
    , \ firstname' -> athleteDetailed { athleteDetailed_firstname = firstname' }
    )

instance FirstnameLens AthleteSummary Text where
  firstname athleteSummary =
    ( athleteSummary_firstname athleteSummary
    , \ firstname' -> athleteSummary { athleteSummary_firstname = firstname' }
    )

instance FlaggedLens ActivityDetailed Bool where
  flagged activityDetailed =
    ( activityDetailed_flagged activityDetailed
    , \ flagged' -> activityDetailed { activityDetailed_flagged = flagged' }
    )

instance FlaggedLens ActivitySummary Bool where
  flagged activitySummary =
    ( activitySummary_flagged activitySummary
    , \ flagged' -> activitySummary { activitySummary_flagged = flagged' }
    )

instance FollowerCountLens AthleteDetailed Integer where
  followerCount athleteDetailed =
    ( athleteDetailed_followerCount athleteDetailed
    , \ followerCount' -> athleteDetailed { athleteDetailed_followerCount = followerCount' }
    )

instance FollowerLens AthleteDetailed (Maybe Text) where
  follower athleteDetailed =
    ( athleteDetailed_follower athleteDetailed
    , \ follower' -> athleteDetailed { athleteDetailed_follower = follower' }
    )

instance FollowerLens AthleteSummary (Maybe Text) where
  follower athleteSummary =
    ( athleteSummary_follower athleteSummary
    , \ follower' -> athleteSummary { athleteSummary_follower = follower' }
    )

instance FrameTypeLens GearDetailed (Maybe Integer) where
  frameType gearDetailed =
    ( gearDetailed_frameType gearDetailed
    , \ frameType' -> gearDetailed { gearDetailed_frameType = frameType' }
    )

instance FriendCountLens AthleteDetailed Integer where
  friendCount athleteDetailed =
    ( athleteDetailed_friendCount athleteDetailed
    , \ friendCount' -> athleteDetailed { athleteDetailed_friendCount = friendCount' }
    )

instance FriendLens AthleteDetailed (Maybe Text) where
  friend athleteDetailed =
    ( athleteDetailed_friend athleteDetailed
    , \ friend' -> athleteDetailed { athleteDetailed_friend = friend' }
    )

instance FriendLens AthleteSummary (Maybe Text) where
  friend athleteSummary =
    ( athleteSummary_friend athleteSummary
    , \ friend' -> athleteSummary { athleteSummary_friend = friend' }
    )

instance FtpLens AthleteDetailed (Maybe Integer) where
  ftp athleteDetailed =
    ( athleteDetailed_ftp athleteDetailed
    , \ ftp' -> athleteDetailed { athleteDetailed_ftp = ftp' }
    )

instance GearIdLens ActivityDetailed (Maybe Text) where
  gearId activityDetailed =
    ( activityDetailed_gearId activityDetailed
    , \ gearId' -> activityDetailed { activityDetailed_gearId = gearId' }
    )

instance GearIdLens ActivitySummary (Maybe Text) where
  gearId activitySummary =
    ( activitySummary_gearId activitySummary
    , \ gearId' -> activitySummary { activitySummary_gearId = gearId' }
    )

instance GearIdLens UpdateActivityOptions (Maybe String) where
  gearId updateActivityOptions =
    ( updateActivityOptions_gearId updateActivityOptions
    , \ gearId' -> updateActivityOptions { updateActivityOptions_gearId = gearId' }
    )

instance GearLens ActivityDetailed GearSummary where
  gear activityDetailed =
    ( activityDetailed_gear activityDetailed
    , \ gear' -> activityDetailed { activityDetailed_gear = gear' }
    )

instance HasKudoedLens ActivityDetailed Bool where
  hasKudoed activityDetailed =
    ( activityDetailed_hasKudoed activityDetailed
    , \ hasKudoed' -> activityDetailed { activityDetailed_hasKudoed = hasKudoed' }
    )

instance HasKudoedLens ActivitySummary Bool where
  hasKudoed activitySummary =
    ( activitySummary_hasKudoed activitySummary
    , \ hasKudoed' -> activitySummary { activitySummary_hasKudoed = hasKudoed' }
    )

instance HazardousLens SegmentDetailed Bool where
  hazardous segmentDetailed =
    ( segmentDetailed_hazardous segmentDetailed
    , \ hazardous' -> segmentDetailed { segmentDetailed_hazardous = hazardous' }
    )

instance HiddenLens EffortDetailed (Maybe Bool) where
  hidden effortDetailed =
    ( effortDetailed_hidden effortDetailed
    , \ hidden' -> effortDetailed { effortDetailed_hidden = hidden' }
    )

instance HttpManagerLens Client Manager where
  httpManager client =
    ( client_httpManager client
    , \ httpManager' -> client { client_httpManager = httpManager' }
    )

instance IdLens ActivityDetailed Integer where
  id activityDetailed =
    ( activityDetailed_id activityDetailed
    , \ id' -> activityDetailed { activityDetailed_id = id' }
    )

instance IdLens ActivityLapSummary Integer where
  id activityLapSummary =
    ( activityLapSummary_id activityLapSummary
    , \ id' -> activityLapSummary { activityLapSummary_id = id' }
    )

instance IdLens ActivitySummary Integer where
  id activitySummary =
    ( activitySummary_id activitySummary
    , \ id' -> activitySummary { activitySummary_id = id' }
    )

instance IdLens AthleteDetailed Integer where
  id athleteDetailed =
    ( athleteDetailed_id athleteDetailed
    , \ id' -> athleteDetailed { athleteDetailed_id = id' }
    )

instance IdLens AthleteMeta Integer where
  id athleteMeta =
    ( athleteMeta_id athleteMeta
    , \ id' -> athleteMeta { athleteMeta_id = id' }
    )

instance IdLens AthleteSummary Integer where
  id athleteSummary =
    ( athleteSummary_id athleteSummary
    , \ id' -> athleteSummary { athleteSummary_id = id' }
    )

instance IdLens ClubDetailed Integer where
  id clubDetailed =
    ( clubDetailed_id clubDetailed
    , \ id' -> clubDetailed { clubDetailed_id = id' }
    )

instance IdLens ClubSummary Integer where
  id clubSummary =
    ( clubSummary_id clubSummary
    , \ id' -> clubSummary { clubSummary_id = id' }
    )

instance IdLens CommentSummary Integer where
  id commentSummary =
    ( commentSummary_id commentSummary
    , \ id' -> commentSummary { commentSummary_id = id' }
    )

instance IdLens EffortDetailed Integer where
  id effortDetailed =
    ( effortDetailed_id effortDetailed
    , \ id' -> effortDetailed { effortDetailed_id = id' }
    )

instance IdLens GearDetailed Text where
  id gearDetailed =
    ( gearDetailed_id gearDetailed
    , \ id' -> gearDetailed { gearDetailed_id = id' }
    )

instance IdLens GearSummary Text where
  id gearSummary =
    ( gearSummary_id gearSummary
    , \ id' -> gearSummary { gearSummary_id = id' }
    )

instance IdLens PhotoSummary Integer where
  id photoSummary =
    ( photoSummary_id photoSummary
    , \ id' -> photoSummary { photoSummary_id = id' }
    )

instance IdLens PolylineDetailed Text where
  id polylineDetailed =
    ( polylineDetailed_id polylineDetailed
    , \ id' -> polylineDetailed { polylineDetailed_id = id' }
    )

instance IdLens PolylineSummary Text where
  id polylineSummary =
    ( polylineSummary_id polylineSummary
    , \ id' -> polylineSummary { polylineSummary_id = id' }
    )

instance IdLens SegmentDetailed Integer where
  id segmentDetailed =
    ( segmentDetailed_id segmentDetailed
    , \ id' -> segmentDetailed { segmentDetailed_id = id' }
    )

instance IdLens SegmentSummary Integer where
  id segmentSummary =
    ( segmentSummary_id segmentSummary
    , \ id' -> segmentSummary { segmentSummary_id = id' }
    )

instance InstagramPrimaryPhotoLens ActivityDetailed Text where
  instagramPrimaryPhoto activityDetailed =
    ( activityDetailed_instagramPrimaryPhoto activityDetailed
    , \ instagramPrimaryPhoto' -> activityDetailed { activityDetailed_instagramPrimaryPhoto = instagramPrimaryPhoto' }
    )

instance KilojoulesLens ActivityDetailed (Maybe Double) where
  kilojoules activityDetailed =
    ( activityDetailed_kilojoules activityDetailed
    , \ kilojoules' -> activityDetailed { activityDetailed_kilojoules = kilojoules' }
    )

instance KilojoulesLens ActivitySummary (Maybe Double) where
  kilojoules activitySummary =
    ( activitySummary_kilojoules activitySummary
    , \ kilojoules' -> activitySummary { activitySummary_kilojoules = kilojoules' }
    )

instance KomRankLens EffortDetailed (Maybe Integer) where
  komRank effortDetailed =
    ( effortDetailed_komRank effortDetailed
    , \ komRank' -> effortDetailed { effortDetailed_komRank = komRank' }
    )

instance KudosCountLens ActivitySummary Integer where
  kudosCount activitySummary =
    ( activitySummary_kudosCount activitySummary
    , \ kudosCount' -> activitySummary { activitySummary_kudosCount = kudosCount' }
    )

instance LapIndexLens ActivityLapSummary Integer where
  lapIndex activityLapSummary =
    ( activityLapSummary_lapIndex activityLapSummary
    , \ lapIndex' -> activityLapSummary { activityLapSummary_lapIndex = lapIndex' }
    )

instance LastnameLens AthleteDetailed Text where
  lastname athleteDetailed =
    ( athleteDetailed_lastname athleteDetailed
    , \ lastname' -> athleteDetailed { athleteDetailed_lastname = lastname' }
    )

instance LastnameLens AthleteSummary Text where
  lastname athleteSummary =
    ( athleteSummary_lastname athleteSummary
    , \ lastname' -> athleteSummary { athleteSummary_lastname = lastname' }
    )

instance LocationCityLens ActivityDetailed (Maybe Text) where
  locationCity activityDetailed =
    ( activityDetailed_locationCity activityDetailed
    , \ locationCity' -> activityDetailed { activityDetailed_locationCity = locationCity' }
    )

instance LocationCityLens ActivitySummary (Maybe Text) where
  locationCity activitySummary =
    ( activitySummary_locationCity activitySummary
    , \ locationCity' -> activitySummary { activitySummary_locationCity = locationCity' }
    )

instance LocationCountryLens ActivityDetailed Text where
  locationCountry activityDetailed =
    ( activityDetailed_locationCountry activityDetailed
    , \ locationCountry' -> activityDetailed { activityDetailed_locationCountry = locationCountry' }
    )

instance LocationCountryLens ActivitySummary Text where
  locationCountry activitySummary =
    ( activitySummary_locationCountry activitySummary
    , \ locationCountry' -> activitySummary { activitySummary_locationCountry = locationCountry' }
    )

instance LocationLens PhotoSummary (Maybe (Double, Double)) where
  location photoSummary =
    ( photoSummary_location photoSummary
    , \ location' -> photoSummary { photoSummary_location = location' }
    )

instance LocationStateLens ActivityDetailed (Maybe Text) where
  locationState activityDetailed =
    ( activityDetailed_locationState activityDetailed
    , \ locationState' -> activityDetailed { activityDetailed_locationState = locationState' }
    )

instance LocationStateLens ActivitySummary (Maybe Text) where
  locationState activitySummary =
    ( activitySummary_locationState activitySummary
    , \ locationState' -> activitySummary { activitySummary_locationState = locationState' }
    )

instance ManualLens ActivityDetailed Bool where
  manual activityDetailed =
    ( activityDetailed_manual activityDetailed
    , \ manual' -> activityDetailed { activityDetailed_manual = manual' }
    )

instance ManualLens ActivitySummary Bool where
  manual activitySummary =
    ( activitySummary_manual activitySummary
    , \ manual' -> activitySummary { activitySummary_manual = manual' }
    )

instance MapLens ActivityDetailed PolylineDetailed where
  map activityDetailed =
    ( activityDetailed_map activityDetailed
    , \ map' -> activityDetailed { activityDetailed_map = map' }
    )

instance MapLens ActivitySummary PolylineSummary where
  map activitySummary =
    ( activitySummary_map activitySummary
    , \ map' -> activitySummary { activitySummary_map = map' }
    )

instance MapLens SegmentDetailed PolylineDetailed where
  map segmentDetailed =
    ( segmentDetailed_map segmentDetailed
    , \ map' -> segmentDetailed { segmentDetailed_map = map' }
    )

instance MarkdownLens GetActivityCommentsOptions Bool where
  markdown getActivityCommentsOptions =
    ( getActivityCommentsOptions_markdown getActivityCommentsOptions
    , \ markdown' -> getActivityCommentsOptions { getActivityCommentsOptions_markdown = markdown' }
    )

instance MaxHeartrateLens EffortDetailed (Maybe Integer) where
  maxHeartrate effortDetailed =
    ( effortDetailed_maxHeartrate effortDetailed
    , \ maxHeartrate' -> effortDetailed { effortDetailed_maxHeartrate = maxHeartrate' }
    )

instance MaxLens ActivityZoneDistributionBucket Integer where
  max activityZoneDistributionBucket =
    ( activityZoneDistributionBucket_max activityZoneDistributionBucket
    , \ max' -> activityZoneDistributionBucket { activityZoneDistributionBucket_max = max' }
    )

instance MaxSpeedLens ActivityDetailed Double where
  maxSpeed activityDetailed =
    ( activityDetailed_maxSpeed activityDetailed
    , \ maxSpeed' -> activityDetailed { activityDetailed_maxSpeed = maxSpeed' }
    )

instance MaxSpeedLens ActivityLapSummary Double where
  maxSpeed activityLapSummary =
    ( activityLapSummary_maxSpeed activityLapSummary
    , \ maxSpeed' -> activityLapSummary { activityLapSummary_maxSpeed = maxSpeed' }
    )

instance MaxSpeedLens ActivitySummary Double where
  maxSpeed activitySummary =
    ( activitySummary_maxSpeed activitySummary
    , \ maxSpeed' -> activitySummary { activitySummary_maxSpeed = maxSpeed' }
    )

instance MaximumGradeLens SegmentDetailed Double where
  maximumGrade segmentDetailed =
    ( segmentDetailed_maximumGrade segmentDetailed
    , \ maximumGrade' -> segmentDetailed { segmentDetailed_maximumGrade = maximumGrade' }
    )

instance MaximumGradeLens SegmentSummary Double where
  maximumGrade segmentSummary =
    ( segmentSummary_maximumGrade segmentSummary
    , \ maximumGrade' -> segmentSummary { segmentSummary_maximumGrade = maximumGrade' }
    )

instance MeasurementPreferenceLens AthleteDetailed Text where
  measurementPreference athleteDetailed =
    ( athleteDetailed_measurementPreference athleteDetailed
    , \ measurementPreference' -> athleteDetailed { athleteDetailed_measurementPreference = measurementPreference' }
    )

instance MemberCountLens ClubDetailed Integer where
  memberCount clubDetailed =
    ( clubDetailed_memberCount clubDetailed
    , \ memberCount' -> clubDetailed { clubDetailed_memberCount = memberCount' }
    )

instance MinLens ActivityZoneDistributionBucket Integer where
  min activityZoneDistributionBucket =
    ( activityZoneDistributionBucket_min activityZoneDistributionBucket
    , \ min' -> activityZoneDistributionBucket { activityZoneDistributionBucket_min = min' }
    )

instance ModelNameLens GearDetailed Text where
  modelName gearDetailed =
    ( gearDetailed_modelName gearDetailed
    , \ modelName' -> gearDetailed { gearDetailed_modelName = modelName' }
    )

instance MovingTimeLens ActivityDetailed Integer where
  movingTime activityDetailed =
    ( activityDetailed_movingTime activityDetailed
    , \ movingTime' -> activityDetailed { activityDetailed_movingTime = movingTime' }
    )

instance MovingTimeLens ActivityLapSummary Double where
  movingTime activityLapSummary =
    ( activityLapSummary_movingTime activityLapSummary
    , \ movingTime' -> activityLapSummary { activityLapSummary_movingTime = movingTime' }
    )

instance MovingTimeLens ActivitySummary Integer where
  movingTime activitySummary =
    ( activitySummary_movingTime activitySummary
    , \ movingTime' -> activitySummary { activitySummary_movingTime = movingTime' }
    )

instance MovingTimeLens EffortDetailed Integer where
  movingTime effortDetailed =
    ( effortDetailed_movingTime effortDetailed
    , \ movingTime' -> effortDetailed { effortDetailed_movingTime = movingTime' }
    )

instance MutualFriendCountLens AthleteDetailed Integer where
  mutualFriendCount athleteDetailed =
    ( athleteDetailed_mutualFriendCount athleteDetailed
    , \ mutualFriendCount' -> athleteDetailed { athleteDetailed_mutualFriendCount = mutualFriendCount' }
    )

instance NameLens ActivityDetailed Text where
  name activityDetailed =
    ( activityDetailed_name activityDetailed
    , \ name' -> activityDetailed { activityDetailed_name = name' }
    )

instance NameLens ActivityLapSummary Text where
  name activityLapSummary =
    ( activityLapSummary_name activityLapSummary
    , \ name' -> activityLapSummary { activityLapSummary_name = name' }
    )

instance NameLens ActivitySummary Text where
  name activitySummary =
    ( activitySummary_name activitySummary
    , \ name' -> activitySummary { activitySummary_name = name' }
    )

instance NameLens ClubDetailed Text where
  name clubDetailed =
    ( clubDetailed_name clubDetailed
    , \ name' -> clubDetailed { clubDetailed_name = name' }
    )

instance NameLens ClubSummary Text where
  name clubSummary =
    ( clubSummary_name clubSummary
    , \ name' -> clubSummary { clubSummary_name = name' }
    )

instance NameLens EffortDetailed Text where
  name effortDetailed =
    ( effortDetailed_name effortDetailed
    , \ name' -> effortDetailed { effortDetailed_name = name' }
    )

instance NameLens GearDetailed Text where
  name gearDetailed =
    ( gearDetailed_name gearDetailed
    , \ name' -> gearDetailed { gearDetailed_name = name' }
    )

instance NameLens GearSummary Text where
  name gearSummary =
    ( gearSummary_name gearSummary
    , \ name' -> gearSummary { gearSummary_name = name' }
    )

instance NameLens SegmentDetailed Text where
  name segmentDetailed =
    ( segmentDetailed_name segmentDetailed
    , \ name' -> segmentDetailed { segmentDetailed_name = name' }
    )

instance NameLens SegmentSummary Text where
  name segmentSummary =
    ( segmentSummary_name segmentSummary
    , \ name' -> segmentSummary { segmentSummary_name = name' }
    )

instance NameLens UpdateActivityOptions (Maybe String) where
  name updateActivityOptions =
    ( updateActivityOptions_name updateActivityOptions
    , \ name' -> updateActivityOptions { updateActivityOptions_name = name' }
    )

instance PageLens GetActivityCommentsOptions Integer where
  page getActivityCommentsOptions =
    ( getActivityCommentsOptions_page getActivityCommentsOptions
    , \ page' -> getActivityCommentsOptions { getActivityCommentsOptions_page = page' }
    )

instance PageLens GetActivityKudoersOptions Integer where
  page getActivityKudoersOptions =
    ( getActivityKudoersOptions_page getActivityKudoersOptions
    , \ page' -> getActivityKudoersOptions { getActivityKudoersOptions_page = page' }
    )

instance PageLens GetAthleteCrsOptions Integer where
  page getAthleteCrsOptions =
    ( getAthleteCrsOptions_page getAthleteCrsOptions
    , \ page' -> getAthleteCrsOptions { getAthleteCrsOptions_page = page' }
    )

instance PageLens GetClubActivitiesOptions Integer where
  page getClubActivitiesOptions =
    ( getClubActivitiesOptions_page getClubActivitiesOptions
    , \ page' -> getClubActivitiesOptions { getClubActivitiesOptions_page = page' }
    )

instance PageLens GetClubMembersOptions Integer where
  page getClubMembersOptions =
    ( getClubMembersOptions_page getClubMembersOptions
    , \ page' -> getClubMembersOptions { getClubMembersOptions_page = page' }
    )

instance PageLens GetCommonFriendsOptions Integer where
  page getCommonFriendsOptions =
    ( getCommonFriendsOptions_page getCommonFriendsOptions
    , \ page' -> getCommonFriendsOptions { getCommonFriendsOptions_page = page' }
    )

instance PageLens GetCurrentActivitiesOptions Integer where
  page getCurrentActivitiesOptions =
    ( getCurrentActivitiesOptions_page getCurrentActivitiesOptions
    , \ page' -> getCurrentActivitiesOptions { getCurrentActivitiesOptions_page = page' }
    )

instance PageLens GetCurrentFollowersOptions Integer where
  page getCurrentFollowersOptions =
    ( getCurrentFollowersOptions_page getCurrentFollowersOptions
    , \ page' -> getCurrentFollowersOptions { getCurrentFollowersOptions_page = page' }
    )

instance PageLens GetCurrentFriendsOptions Integer where
  page getCurrentFriendsOptions =
    ( getCurrentFriendsOptions_page getCurrentFriendsOptions
    , \ page' -> getCurrentFriendsOptions { getCurrentFriendsOptions_page = page' }
    )

instance PageLens GetFeedOptions Integer where
  page getFeedOptions =
    ( getFeedOptions_page getFeedOptions
    , \ page' -> getFeedOptions { getFeedOptions_page = page' }
    )

instance PageLens GetFollowersOptions Integer where
  page getFollowersOptions =
    ( getFollowersOptions_page getFollowersOptions
    , \ page' -> getFollowersOptions { getFollowersOptions_page = page' }
    )

instance PageLens GetFriendsOptions Integer where
  page getFriendsOptions =
    ( getFriendsOptions_page getFriendsOptions
    , \ page' -> getFriendsOptions { getFriendsOptions_page = page' }
    )

instance PageLens GetStarredSegmentsOptions Integer where
  page getStarredSegmentsOptions =
    ( getStarredSegmentsOptions_page getStarredSegmentsOptions
    , \ page' -> getStarredSegmentsOptions { getStarredSegmentsOptions_page = page' }
    )

instance PerPageLens GetActivityCommentsOptions Integer where
  perPage getActivityCommentsOptions =
    ( getActivityCommentsOptions_perPage getActivityCommentsOptions
    , \ perPage' -> getActivityCommentsOptions { getActivityCommentsOptions_perPage = perPage' }
    )

instance PerPageLens GetActivityKudoersOptions Integer where
  perPage getActivityKudoersOptions =
    ( getActivityKudoersOptions_perPage getActivityKudoersOptions
    , \ perPage' -> getActivityKudoersOptions { getActivityKudoersOptions_perPage = perPage' }
    )

instance PerPageLens GetAthleteCrsOptions Integer where
  perPage getAthleteCrsOptions =
    ( getAthleteCrsOptions_perPage getAthleteCrsOptions
    , \ perPage' -> getAthleteCrsOptions { getAthleteCrsOptions_perPage = perPage' }
    )

instance PerPageLens GetClubActivitiesOptions Integer where
  perPage getClubActivitiesOptions =
    ( getClubActivitiesOptions_perPage getClubActivitiesOptions
    , \ perPage' -> getClubActivitiesOptions { getClubActivitiesOptions_perPage = perPage' }
    )

instance PerPageLens GetClubMembersOptions Integer where
  perPage getClubMembersOptions =
    ( getClubMembersOptions_perPage getClubMembersOptions
    , \ perPage' -> getClubMembersOptions { getClubMembersOptions_perPage = perPage' }
    )

instance PerPageLens GetCommonFriendsOptions Integer where
  perPage getCommonFriendsOptions =
    ( getCommonFriendsOptions_perPage getCommonFriendsOptions
    , \ perPage' -> getCommonFriendsOptions { getCommonFriendsOptions_perPage = perPage' }
    )

instance PerPageLens GetCurrentActivitiesOptions Integer where
  perPage getCurrentActivitiesOptions =
    ( getCurrentActivitiesOptions_perPage getCurrentActivitiesOptions
    , \ perPage' -> getCurrentActivitiesOptions { getCurrentActivitiesOptions_perPage = perPage' }
    )

instance PerPageLens GetCurrentFollowersOptions Integer where
  perPage getCurrentFollowersOptions =
    ( getCurrentFollowersOptions_perPage getCurrentFollowersOptions
    , \ perPage' -> getCurrentFollowersOptions { getCurrentFollowersOptions_perPage = perPage' }
    )

instance PerPageLens GetCurrentFriendsOptions Integer where
  perPage getCurrentFriendsOptions =
    ( getCurrentFriendsOptions_perPage getCurrentFriendsOptions
    , \ perPage' -> getCurrentFriendsOptions { getCurrentFriendsOptions_perPage = perPage' }
    )

instance PerPageLens GetFeedOptions Integer where
  perPage getFeedOptions =
    ( getFeedOptions_perPage getFeedOptions
    , \ perPage' -> getFeedOptions { getFeedOptions_perPage = perPage' }
    )

instance PerPageLens GetFollowersOptions Integer where
  perPage getFollowersOptions =
    ( getFollowersOptions_perPage getFollowersOptions
    , \ perPage' -> getFollowersOptions { getFollowersOptions_perPage = perPage' }
    )

instance PerPageLens GetFriendsOptions Integer where
  perPage getFriendsOptions =
    ( getFriendsOptions_perPage getFriendsOptions
    , \ perPage' -> getFriendsOptions { getFriendsOptions_perPage = perPage' }
    )

instance PerPageLens GetStarredSegmentsOptions Integer where
  perPage getStarredSegmentsOptions =
    ( getStarredSegmentsOptions_perPage getStarredSegmentsOptions
    , \ perPage' -> getStarredSegmentsOptions { getStarredSegmentsOptions_perPage = perPage' }
    )

instance PhotoCountLens ActivityDetailed Integer where
  photoCount activityDetailed =
    ( activityDetailed_photoCount activityDetailed
    , \ photoCount' -> activityDetailed { activityDetailed_photoCount = photoCount' }
    )

instance PhotoCountLens ActivitySummary Integer where
  photoCount activitySummary =
    ( activitySummary_photoCount activitySummary
    , \ photoCount' -> activitySummary { activitySummary_photoCount = photoCount' }
    )

instance PolylineLens PolylineDetailed ([(Double, Double)]) where
  polyline polylineDetailed =
    ( polylineDetailed_polyline polylineDetailed
    , \ polyline' -> polylineDetailed { polylineDetailed_polyline = polyline' }
    )

instance PrRankLens EffortDetailed (Maybe Integer) where
  prRank effortDetailed =
    ( effortDetailed_prRank effortDetailed
    , \ prRank' -> effortDetailed { effortDetailed_prRank = prRank' }
    )

instance PremiumLens AthleteDetailed Bool where
  premium athleteDetailed =
    ( athleteDetailed_premium athleteDetailed
    , \ premium' -> athleteDetailed { athleteDetailed_premium = premium' }
    )

instance PremiumLens AthleteSummary Bool where
  premium athleteSummary =
    ( athleteSummary_premium athleteSummary
    , \ premium' -> athleteSummary { athleteSummary_premium = premium' }
    )

instance PrimaryLens GearDetailed Bool where
  primary gearDetailed =
    ( gearDetailed_primary gearDetailed
    , \ primary' -> gearDetailed { gearDetailed_primary = primary' }
    )

instance PrimaryLens GearSummary Bool where
  primary gearSummary =
    ( gearSummary_primary gearSummary
    , \ primary' -> gearSummary { gearSummary_primary = primary' }
    )

instance PrivateLens ActivityDetailed Bool where
  private activityDetailed =
    ( activityDetailed_private activityDetailed
    , \ private' -> activityDetailed { activityDetailed_private = private' }
    )

instance PrivateLens ActivitySummary Bool where
  private activitySummary =
    ( activitySummary_private activitySummary
    , \ private' -> activitySummary { activitySummary_private = private' }
    )

instance PrivateLens ClubDetailed Bool where
  private clubDetailed =
    ( clubDetailed_private clubDetailed
    , \ private' -> clubDetailed { clubDetailed_private = private' }
    )

instance PrivateLens SegmentDetailed Bool where
  private segmentDetailed =
    ( segmentDetailed_private segmentDetailed
    , \ private' -> segmentDetailed { segmentDetailed_private = private' }
    )

instance PrivateLens SegmentSummary Bool where
  private segmentSummary =
    ( segmentSummary_private segmentSummary
    , \ private' -> segmentSummary { segmentSummary_private = private' }
    )

instance PrivateLens UpdateActivityOptions (Maybe Bool) where
  private updateActivityOptions =
    ( updateActivityOptions_private updateActivityOptions
    , \ private' -> updateActivityOptions { updateActivityOptions_private = private' }
    )

instance PrivateScopeLens BuildAuthorizeUrlOptions Bool where
  privateScope buildAuthorizeUrlOptions =
    ( buildAuthorizeUrlOptions_privateScope buildAuthorizeUrlOptions
    , \ privateScope' -> buildAuthorizeUrlOptions { buildAuthorizeUrlOptions_privateScope = privateScope' }
    )

instance ProfileLens AthleteDetailed Text where
  profile athleteDetailed =
    ( athleteDetailed_profile athleteDetailed
    , \ profile' -> athleteDetailed { athleteDetailed_profile = profile' }
    )

instance ProfileLens AthleteSummary Text where
  profile athleteSummary =
    ( athleteSummary_profile athleteSummary
    , \ profile' -> athleteSummary { athleteSummary_profile = profile' }
    )

instance ProfileLens ClubDetailed Text where
  profile clubDetailed =
    ( clubDetailed_profile clubDetailed
    , \ profile' -> clubDetailed { clubDetailed_profile = profile' }
    )

instance ProfileLens ClubSummary Text where
  profile clubSummary =
    ( clubSummary_profile clubSummary
    , \ profile' -> clubSummary { clubSummary_profile = profile' }
    )

instance ProfileMediumLens AthleteDetailed Text where
  profileMedium athleteDetailed =
    ( athleteDetailed_profileMedium athleteDetailed
    , \ profileMedium' -> athleteDetailed { athleteDetailed_profileMedium = profileMedium' }
    )

instance ProfileMediumLens AthleteSummary Text where
  profileMedium athleteSummary =
    ( athleteSummary_profileMedium athleteSummary
    , \ profileMedium' -> athleteSummary { athleteSummary_profileMedium = profileMedium' }
    )

instance ProfileMediumLens ClubDetailed Text where
  profileMedium clubDetailed =
    ( clubDetailed_profileMedium clubDetailed
    , \ profileMedium' -> clubDetailed { clubDetailed_profileMedium = profileMedium' }
    )

instance ProfileMediumLens ClubSummary Text where
  profileMedium clubSummary =
    ( clubSummary_profileMedium clubSummary
    , \ profileMedium' -> clubSummary { clubSummary_profileMedium = profileMedium' }
    )

instance RefLens PhotoSummary Text where
  ref photoSummary =
    ( photoSummary_ref photoSummary
    , \ ref' -> photoSummary { photoSummary_ref = ref' }
    )

instance ResourceStateLens ActivityDetailed Integer where
  resourceState activityDetailed =
    ( activityDetailed_resourceState activityDetailed
    , \ resourceState' -> activityDetailed { activityDetailed_resourceState = resourceState' }
    )

instance ResourceStateLens ActivityLapSummary Integer where
  resourceState activityLapSummary =
    ( activityLapSummary_resourceState activityLapSummary
    , \ resourceState' -> activityLapSummary { activityLapSummary_resourceState = resourceState' }
    )

instance ResourceStateLens ActivitySummary Integer where
  resourceState activitySummary =
    ( activitySummary_resourceState activitySummary
    , \ resourceState' -> activitySummary { activitySummary_resourceState = resourceState' }
    )

instance ResourceStateLens ActivityZoneDetailed Integer where
  resourceState activityZoneDetailed =
    ( activityZoneDetailed_resourceState activityZoneDetailed
    , \ resourceState' -> activityZoneDetailed { activityZoneDetailed_resourceState = resourceState' }
    )

instance ResourceStateLens AthleteDetailed Integer where
  resourceState athleteDetailed =
    ( athleteDetailed_resourceState athleteDetailed
    , \ resourceState' -> athleteDetailed { athleteDetailed_resourceState = resourceState' }
    )

instance ResourceStateLens AthleteMeta Integer where
  resourceState athleteMeta =
    ( athleteMeta_resourceState athleteMeta
    , \ resourceState' -> athleteMeta { athleteMeta_resourceState = resourceState' }
    )

instance ResourceStateLens AthleteSummary Integer where
  resourceState athleteSummary =
    ( athleteSummary_resourceState athleteSummary
    , \ resourceState' -> athleteSummary { athleteSummary_resourceState = resourceState' }
    )

instance ResourceStateLens ClubDetailed Integer where
  resourceState clubDetailed =
    ( clubDetailed_resourceState clubDetailed
    , \ resourceState' -> clubDetailed { clubDetailed_resourceState = resourceState' }
    )

instance ResourceStateLens ClubSummary Integer where
  resourceState clubSummary =
    ( clubSummary_resourceState clubSummary
    , \ resourceState' -> clubSummary { clubSummary_resourceState = resourceState' }
    )

instance ResourceStateLens CommentSummary Integer where
  resourceState commentSummary =
    ( commentSummary_resourceState commentSummary
    , \ resourceState' -> commentSummary { commentSummary_resourceState = resourceState' }
    )

instance ResourceStateLens EffortDetailed Integer where
  resourceState effortDetailed =
    ( effortDetailed_resourceState effortDetailed
    , \ resourceState' -> effortDetailed { effortDetailed_resourceState = resourceState' }
    )

instance ResourceStateLens GearDetailed Integer where
  resourceState gearDetailed =
    ( gearDetailed_resourceState gearDetailed
    , \ resourceState' -> gearDetailed { gearDetailed_resourceState = resourceState' }
    )

instance ResourceStateLens GearSummary Integer where
  resourceState gearSummary =
    ( gearSummary_resourceState gearSummary
    , \ resourceState' -> gearSummary { gearSummary_resourceState = resourceState' }
    )

instance ResourceStateLens PhotoSummary Integer where
  resourceState photoSummary =
    ( photoSummary_resourceState photoSummary
    , \ resourceState' -> photoSummary { photoSummary_resourceState = resourceState' }
    )

instance ResourceStateLens PolylineDetailed Integer where
  resourceState polylineDetailed =
    ( polylineDetailed_resourceState polylineDetailed
    , \ resourceState' -> polylineDetailed { polylineDetailed_resourceState = resourceState' }
    )

instance ResourceStateLens PolylineSummary Integer where
  resourceState polylineSummary =
    ( polylineSummary_resourceState polylineSummary
    , \ resourceState' -> polylineSummary { polylineSummary_resourceState = resourceState' }
    )

instance ResourceStateLens SegmentDetailed Integer where
  resourceState segmentDetailed =
    ( segmentDetailed_resourceState segmentDetailed
    , \ resourceState' -> segmentDetailed { segmentDetailed_resourceState = resourceState' }
    )

instance ResourceStateLens SegmentSummary Integer where
  resourceState segmentSummary =
    ( segmentSummary_resourceState segmentSummary
    , \ resourceState' -> segmentSummary { segmentSummary_resourceState = resourceState' }
    )

instance SegmentEffortsLens ActivityDetailed [EffortDetailed] where
  segmentEfforts activityDetailed =
    ( activityDetailed_segmentEfforts activityDetailed
    , \ segmentEfforts' -> activityDetailed { activityDetailed_segmentEfforts = segmentEfforts' }
    )

instance SegmentLens EffortDetailed SegmentSummary where
  segment effortDetailed =
    ( effortDetailed_segment effortDetailed
    , \ segment' -> effortDetailed { effortDetailed_segment = segment' }
    )

instance SensorBasedLens ActivityZoneDetailed Bool where
  sensorBased activityZoneDetailed =
    ( activityZoneDetailed_sensorBased activityZoneDetailed
    , \ sensorBased' -> activityZoneDetailed { activityZoneDetailed_sensorBased = sensorBased' }
    )

instance SexLens AthleteDetailed (Maybe Char) where
  sex athleteDetailed =
    ( athleteDetailed_sex athleteDetailed
    , \ sex' -> athleteDetailed { athleteDetailed_sex = sex' }
    )

instance SexLens AthleteSummary (Maybe Char) where
  sex athleteSummary =
    ( athleteSummary_sex athleteSummary
    , \ sex' -> athleteSummary { athleteSummary_sex = sex' }
    )

instance SexLens UpdateCurrentAthleteOptions (Maybe Char) where
  sex updateCurrentAthleteOptions =
    ( updateCurrentAthleteOptions_sex updateCurrentAthleteOptions
    , \ sex' -> updateCurrentAthleteOptions { updateCurrentAthleteOptions_sex = sex' }
    )

instance ShoesLens AthleteDetailed [GearSummary] where
  shoes athleteDetailed =
    ( athleteDetailed_shoes athleteDetailed
    , \ shoes' -> athleteDetailed { athleteDetailed_shoes = shoes' }
    )

instance SportTypeLens ClubDetailed Text where
  sportType clubDetailed =
    ( clubDetailed_sportType clubDetailed
    , \ sportType' -> clubDetailed { clubDetailed_sportType = sportType' }
    )

instance StarCountLens SegmentDetailed Integer where
  starCount segmentDetailed =
    ( segmentDetailed_starCount segmentDetailed
    , \ starCount' -> segmentDetailed { segmentDetailed_starCount = starCount' }
    )

instance StarredLens SegmentDetailed Bool where
  starred segmentDetailed =
    ( segmentDetailed_starred segmentDetailed
    , \ starred' -> segmentDetailed { segmentDetailed_starred = starred' }
    )

instance StarredLens SegmentSummary Bool where
  starred segmentSummary =
    ( segmentSummary_starred segmentSummary
    , \ starred' -> segmentSummary { segmentSummary_starred = starred' }
    )

instance StartDateLens ActivityDetailed UTCTime where
  startDate activityDetailed =
    ( activityDetailed_startDate activityDetailed
    , \ startDate' -> activityDetailed { activityDetailed_startDate = startDate' }
    )

instance StartDateLens ActivityLapSummary UTCTime where
  startDate activityLapSummary =
    ( activityLapSummary_startDate activityLapSummary
    , \ startDate' -> activityLapSummary { activityLapSummary_startDate = startDate' }
    )

instance StartDateLens ActivitySummary UTCTime where
  startDate activitySummary =
    ( activitySummary_startDate activitySummary
    , \ startDate' -> activitySummary { activitySummary_startDate = startDate' }
    )

instance StartDateLens EffortDetailed UTCTime where
  startDate effortDetailed =
    ( effortDetailed_startDate effortDetailed
    , \ startDate' -> effortDetailed { effortDetailed_startDate = startDate' }
    )

instance StartDateLocalLens ActivityDetailed UTCTime where
  startDateLocal activityDetailed =
    ( activityDetailed_startDateLocal activityDetailed
    , \ startDateLocal' -> activityDetailed { activityDetailed_startDateLocal = startDateLocal' }
    )

instance StartDateLocalLens ActivityLapSummary UTCTime where
  startDateLocal activityLapSummary =
    ( activityLapSummary_startDateLocal activityLapSummary
    , \ startDateLocal' -> activityLapSummary { activityLapSummary_startDateLocal = startDateLocal' }
    )

instance StartDateLocalLens ActivitySummary UTCTime where
  startDateLocal activitySummary =
    ( activitySummary_startDateLocal activitySummary
    , \ startDateLocal' -> activitySummary { activitySummary_startDateLocal = startDateLocal' }
    )

instance StartDateLocalLens EffortDetailed UTCTime where
  startDateLocal effortDetailed =
    ( effortDetailed_startDateLocal effortDetailed
    , \ startDateLocal' -> effortDetailed { effortDetailed_startDateLocal = startDateLocal' }
    )

instance StartIndexLens ActivityLapSummary Integer where
  startIndex activityLapSummary =
    ( activityLapSummary_startIndex activityLapSummary
    , \ startIndex' -> activityLapSummary { activityLapSummary_startIndex = startIndex' }
    )

instance StartIndexLens EffortDetailed Integer where
  startIndex effortDetailed =
    ( effortDetailed_startIndex effortDetailed
    , \ startIndex' -> effortDetailed { effortDetailed_startIndex = startIndex' }
    )

instance StartLatitudeLens ActivityDetailed Double where
  startLatitude activityDetailed =
    ( activityDetailed_startLatitude activityDetailed
    , \ startLatitude' -> activityDetailed { activityDetailed_startLatitude = startLatitude' }
    )

instance StartLatitudeLens ActivitySummary Double where
  startLatitude activitySummary =
    ( activitySummary_startLatitude activitySummary
    , \ startLatitude' -> activitySummary { activitySummary_startLatitude = startLatitude' }
    )

instance StartLatitudeLens SegmentDetailed Double where
  startLatitude segmentDetailed =
    ( segmentDetailed_startLatitude segmentDetailed
    , \ startLatitude' -> segmentDetailed { segmentDetailed_startLatitude = startLatitude' }
    )

instance StartLatitudeLens SegmentSummary Double where
  startLatitude segmentSummary =
    ( segmentSummary_startLatitude segmentSummary
    , \ startLatitude' -> segmentSummary { segmentSummary_startLatitude = startLatitude' }
    )

instance StartLatlngLens ActivityDetailed (Maybe (Double, Double)) where
  startLatlng activityDetailed =
    ( activityDetailed_startLatlng activityDetailed
    , \ startLatlng' -> activityDetailed { activityDetailed_startLatlng = startLatlng' }
    )

instance StartLatlngLens ActivitySummary (Maybe (Double, Double)) where
  startLatlng activitySummary =
    ( activitySummary_startLatlng activitySummary
    , \ startLatlng' -> activitySummary { activitySummary_startLatlng = startLatlng' }
    )

instance StartLatlngLens SegmentDetailed ((Double, Double)) where
  startLatlng segmentDetailed =
    ( segmentDetailed_startLatlng segmentDetailed
    , \ startLatlng' -> segmentDetailed { segmentDetailed_startLatlng = startLatlng' }
    )

instance StartLatlngLens SegmentSummary ((Double, Double)) where
  startLatlng segmentSummary =
    ( segmentSummary_startLatlng segmentSummary
    , \ startLatlng' -> segmentSummary { segmentSummary_startLatlng = startLatlng' }
    )

instance StartLongitudeLens ActivityDetailed Double where
  startLongitude activityDetailed =
    ( activityDetailed_startLongitude activityDetailed
    , \ startLongitude' -> activityDetailed { activityDetailed_startLongitude = startLongitude' }
    )

instance StartLongitudeLens ActivitySummary Double where
  startLongitude activitySummary =
    ( activitySummary_startLongitude activitySummary
    , \ startLongitude' -> activitySummary { activitySummary_startLongitude = startLongitude' }
    )

instance StartLongitudeLens SegmentDetailed Double where
  startLongitude segmentDetailed =
    ( segmentDetailed_startLongitude segmentDetailed
    , \ startLongitude' -> segmentDetailed { segmentDetailed_startLongitude = startLongitude' }
    )

instance StartLongitudeLens SegmentSummary Double where
  startLongitude segmentSummary =
    ( segmentSummary_startLongitude segmentSummary
    , \ startLongitude' -> segmentSummary { segmentSummary_startLongitude = startLongitude' }
    )

instance StateLens AthleteDetailed Text where
  state athleteDetailed =
    ( athleteDetailed_state athleteDetailed
    , \ state' -> athleteDetailed { athleteDetailed_state = state' }
    )

instance StateLens AthleteSummary Text where
  state athleteSummary =
    ( athleteSummary_state athleteSummary
    , \ state' -> athleteSummary { athleteSummary_state = state' }
    )

instance StateLens BuildAuthorizeUrlOptions String where
  state buildAuthorizeUrlOptions =
    ( buildAuthorizeUrlOptions_state buildAuthorizeUrlOptions
    , \ state' -> buildAuthorizeUrlOptions { buildAuthorizeUrlOptions_state = state' }
    )

instance StateLens ClubDetailed Text where
  state clubDetailed =
    ( clubDetailed_state clubDetailed
    , \ state' -> clubDetailed { clubDetailed_state = state' }
    )

instance StateLens SegmentDetailed Text where
  state segmentDetailed =
    ( segmentDetailed_state segmentDetailed
    , \ state' -> segmentDetailed { segmentDetailed_state = state' }
    )

instance StateLens SegmentSummary Text where
  state segmentSummary =
    ( segmentSummary_state segmentSummary
    , \ state' -> segmentSummary { segmentSummary_state = state' }
    )

instance StateLens UpdateCurrentAthleteOptions (Maybe String) where
  state updateCurrentAthleteOptions =
    ( updateCurrentAthleteOptions_state updateCurrentAthleteOptions
    , \ state' -> updateCurrentAthleteOptions { updateCurrentAthleteOptions_state = state' }
    )

instance SummaryPolylineLens PolylineDetailed (Maybe [(Double, Double)]) where
  summaryPolyline polylineDetailed =
    ( polylineDetailed_summaryPolyline polylineDetailed
    , \ summaryPolyline' -> polylineDetailed { polylineDetailed_summaryPolyline = summaryPolyline' }
    )

instance SummaryPolylineLens PolylineSummary (Maybe [(Double, Double)]) where
  summaryPolyline polylineSummary =
    ( polylineSummary_summaryPolyline polylineSummary
    , \ summaryPolyline' -> polylineSummary { polylineSummary_summaryPolyline = summaryPolyline' }
    )

instance TextLens CommentSummary Text where
  text commentSummary =
    ( commentSummary_text commentSummary
    , \ text' -> commentSummary { commentSummary_text = text' }
    )

instance TimeLens ActivityZoneDistributionBucket Integer where
  time activityZoneDistributionBucket =
    ( activityZoneDistributionBucket_time activityZoneDistributionBucket
    , \ time' -> activityZoneDistributionBucket { activityZoneDistributionBucket_time = time' }
    )

instance TimezoneLens ActivityDetailed Text where
  timezone activityDetailed =
    ( activityDetailed_timezone activityDetailed
    , \ timezone' -> activityDetailed { activityDetailed_timezone = timezone' }
    )

instance TimezoneLens ActivitySummary Text where
  timezone activitySummary =
    ( activitySummary_timezone activitySummary
    , \ timezone' -> activitySummary { activitySummary_timezone = timezone' }
    )

instance TotalElevationGainLens ActivityDetailed Double where
  totalElevationGain activityDetailed =
    ( activityDetailed_totalElevationGain activityDetailed
    , \ totalElevationGain' -> activityDetailed { activityDetailed_totalElevationGain = totalElevationGain' }
    )

instance TotalElevationGainLens ActivityLapSummary Double where
  totalElevationGain activityLapSummary =
    ( activityLapSummary_totalElevationGain activityLapSummary
    , \ totalElevationGain' -> activityLapSummary { activityLapSummary_totalElevationGain = totalElevationGain' }
    )

instance TotalElevationGainLens ActivitySummary Double where
  totalElevationGain activitySummary =
    ( activitySummary_totalElevationGain activitySummary
    , \ totalElevationGain' -> activitySummary { activitySummary_totalElevationGain = totalElevationGain' }
    )

instance TotalElevationGainLens SegmentDetailed Double where
  totalElevationGain segmentDetailed =
    ( segmentDetailed_totalElevationGain segmentDetailed
    , \ totalElevationGain' -> segmentDetailed { segmentDetailed_totalElevationGain = totalElevationGain' }
    )

instance TrainerLens ActivityDetailed Bool where
  trainer activityDetailed =
    ( activityDetailed_trainer activityDetailed
    , \ trainer' -> activityDetailed { activityDetailed_trainer = trainer' }
    )

instance TrainerLens ActivitySummary Bool where
  trainer activitySummary =
    ( activitySummary_trainer activitySummary
    , \ trainer' -> activitySummary { activitySummary_trainer = trainer' }
    )

instance TrainerLens UpdateActivityOptions (Maybe Bool) where
  trainer updateActivityOptions =
    ( updateActivityOptions_trainer updateActivityOptions
    , \ trainer' -> updateActivityOptions { updateActivityOptions_trainer = trainer' }
    )

instance TruncatedLens ActivityDetailed Integer where
  truncated activityDetailed =
    ( activityDetailed_truncated activityDetailed
    , \ truncated' -> activityDetailed { activityDetailed_truncated = truncated' }
    )

instance TypeLens ActivityDetailed Text where
  type_ activityDetailed =
    ( activityDetailed_type activityDetailed
    , \ type_' -> activityDetailed { activityDetailed_type = type_' }
    )

instance TypeLens ActivitySummary Text where
  type_ activitySummary =
    ( activitySummary_type activitySummary
    , \ type_' -> activitySummary { activitySummary_type = type_' }
    )

instance TypeLens ActivityZoneDetailed Text where
  type_ activityZoneDetailed =
    ( activityZoneDetailed_type activityZoneDetailed
    , \ type_' -> activityZoneDetailed { activityZoneDetailed_type = type_' }
    )

instance TypeLens PhotoSummary Text where
  type_ photoSummary =
    ( photoSummary_type photoSummary
    , \ type_' -> photoSummary { photoSummary_type = type_' }
    )

instance TypeLens UpdateActivityOptions (Maybe String) where
  type_ updateActivityOptions =
    ( updateActivityOptions_type updateActivityOptions
    , \ type_' -> updateActivityOptions { updateActivityOptions_type = type_' }
    )

instance UidLens PhotoSummary Text where
  uid photoSummary =
    ( photoSummary_uid photoSummary
    , \ uid' -> photoSummary { photoSummary_uid = uid' }
    )

instance UpdatedAtLens AthleteDetailed UTCTime where
  updatedAt athleteDetailed =
    ( athleteDetailed_updatedAt athleteDetailed
    , \ updatedAt' -> athleteDetailed { athleteDetailed_updatedAt = updatedAt' }
    )

instance UpdatedAtLens AthleteSummary UTCTime where
  updatedAt athleteSummary =
    ( athleteSummary_updatedAt athleteSummary
    , \ updatedAt' -> athleteSummary { athleteSummary_updatedAt = updatedAt' }
    )

instance UpdatedAtLens SegmentDetailed UTCTime where
  updatedAt segmentDetailed =
    ( segmentDetailed_updatedAt segmentDetailed
    , \ updatedAt' -> segmentDetailed { segmentDetailed_updatedAt = updatedAt' }
    )

instance UploadIdLens ActivityDetailed (Maybe Integer) where
  uploadId activityDetailed =
    ( activityDetailed_uploadId activityDetailed
    , \ uploadId' -> activityDetailed { activityDetailed_uploadId = uploadId' }
    )

instance UploadIdLens ActivitySummary (Maybe Integer) where
  uploadId activitySummary =
    ( activitySummary_uploadId activitySummary
    , \ uploadId' -> activitySummary { activitySummary_uploadId = uploadId' }
    )

instance UploadedAtLens PhotoSummary UTCTime where
  uploadedAt photoSummary =
    ( photoSummary_uploadedAt photoSummary
    , \ uploadedAt' -> photoSummary { photoSummary_uploadedAt = uploadedAt' }
    )

instance WeightLens UpdateCurrentAthleteOptions (Maybe Double) where
  weight updateCurrentAthleteOptions =
    ( updateCurrentAthleteOptions_weight updateCurrentAthleteOptions
    , \ weight' -> updateCurrentAthleteOptions { updateCurrentAthleteOptions_weight = weight' }
    )

instance WriteScopeLens BuildAuthorizeUrlOptions Bool where
  writeScope buildAuthorizeUrlOptions =
    ( buildAuthorizeUrlOptions_writeScope buildAuthorizeUrlOptions
    , \ writeScope' -> buildAuthorizeUrlOptions { buildAuthorizeUrlOptions_writeScope = writeScope' }
    )
