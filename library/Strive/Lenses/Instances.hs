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

instance ActivityIdLens EffortDetailed Integer where
  activityId effortDetailed =
    ( effortDetailed_activityId effortDetailed
    , \ activityId' -> effortDetailed { effortDetailed_activityId = activityId' }
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

instance CaloriesLens ActivityDetailed Double where
  calories activityDetailed =
    ( activityDetailed_calories activityDetailed
    , \ calories' -> activityDetailed { activityDetailed_calories = calories' }
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

instance ClimbCategoryLens SegmentSummary Integer where
  climbCategory segmentSummary =
    ( segmentSummary_climbCategory segmentSummary
    , \ climbCategory' -> segmentSummary { segmentSummary_climbCategory = climbCategory' }
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

instance DescriptionLens CreateActivityOptions (Maybe String) where
  description createActivityOptions =
    ( createActivityOptions_description createActivityOptions
    , \ description' -> createActivityOptions { createActivityOptions_description = description' }
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

instance DistanceLens GearSummary Double where
  distance gearSummary =
    ( gearSummary_distance gearSummary
    , \ distance' -> gearSummary { gearSummary_distance = distance' }
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

instance ElevationHighLens SegmentSummary Double where
  elevationHigh segmentSummary =
    ( segmentSummary_elevationHigh segmentSummary
    , \ elevationHigh' -> segmentSummary { segmentSummary_elevationHigh = elevationHigh' }
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

instance EndLatlngLens SegmentSummary ((Double, Double)) where
  endLatlng segmentSummary =
    ( segmentSummary_endLatlng segmentSummary
    , \ endLatlng' -> segmentSummary { segmentSummary_endLatlng = endLatlng' }
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

instance IdLens ClubSummary Integer where
  id clubSummary =
    ( clubSummary_id clubSummary
    , \ id' -> clubSummary { clubSummary_id = id' }
    )

instance IdLens EffortDetailed Integer where
  id effortDetailed =
    ( effortDetailed_id effortDetailed
    , \ id' -> effortDetailed { effortDetailed_id = id' }
    )

instance IdLens GearSummary Text where
  id gearSummary =
    ( gearSummary_id gearSummary
    , \ id' -> gearSummary { gearSummary_id = id' }
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

instance MinLens ActivityZoneDistributionBucket Integer where
  min activityZoneDistributionBucket =
    ( activityZoneDistributionBucket_min activityZoneDistributionBucket
    , \ min' -> activityZoneDistributionBucket { activityZoneDistributionBucket_min = min' }
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

instance NameLens GearSummary Text where
  name gearSummary =
    ( gearSummary_name gearSummary
    , \ name' -> gearSummary { gearSummary_name = name' }
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

instance PageLens GetAthleteCrsOptions Integer where
  page getAthleteCrsOptions =
    ( getAthleteCrsOptions_page getAthleteCrsOptions
    , \ page' -> getAthleteCrsOptions { getAthleteCrsOptions_page = page' }
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

instance PerPageLens GetAthleteCrsOptions Integer where
  perPage getAthleteCrsOptions =
    ( getAthleteCrsOptions_perPage getAthleteCrsOptions
    , \ perPage' -> getAthleteCrsOptions { getAthleteCrsOptions_perPage = perPage' }
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

instance ProfileMediumLens ClubSummary Text where
  profileMedium clubSummary =
    ( clubSummary_profileMedium clubSummary
    , \ profileMedium' -> clubSummary { clubSummary_profileMedium = profileMedium' }
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

instance ResourceStateLens ClubSummary Integer where
  resourceState clubSummary =
    ( clubSummary_resourceState clubSummary
    , \ resourceState' -> clubSummary { clubSummary_resourceState = resourceState' }
    )

instance ResourceStateLens EffortDetailed Integer where
  resourceState effortDetailed =
    ( effortDetailed_resourceState effortDetailed
    , \ resourceState' -> effortDetailed { effortDetailed_resourceState = resourceState' }
    )

instance ResourceStateLens GearSummary Integer where
  resourceState gearSummary =
    ( gearSummary_resourceState gearSummary
    , \ resourceState' -> gearSummary { gearSummary_resourceState = resourceState' }
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

instance TypeLens UpdateActivityOptions (Maybe String) where
  type_ updateActivityOptions =
    ( updateActivityOptions_type updateActivityOptions
    , \ type_' -> updateActivityOptions { updateActivityOptions_type = type_' }
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
