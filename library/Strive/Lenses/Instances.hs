{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Automatically generated lens instances.
module Strive.Lenses.Instances where

import Data.Aeson (Value)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Network.HTTP.Conduit (Manager, Request, Response)
import Strive.Client
import Strive.Enums
import Strive.Internal.Options
import Strive.Lenses.Classes
import Strive.Options
import Strive.Types

instance AccessTokenLens Client String where
  accessToken f x = fmap
    (\ y -> x { client_accessToken = y })
    (f (client_accessToken x))

instance AccessTokenLens DeauthorizationResponse Text where
  accessToken f x = fmap
    (\ y -> x { deauthorizationResponse_accessToken = y })
    (f (deauthorizationResponse_accessToken x))

instance AccessTokenLens TokenExchangeResponse Text where
  accessToken f x = fmap
    (\ y -> x { tokenExchangeResponse_accessToken = y })
    (f (tokenExchangeResponse_accessToken x))

instance AchievementCountLens ActivityDetailed Integer where
  achievementCount f x = fmap
    (\ y -> x { activityDetailed_achievementCount = y })
    (f (activityDetailed_achievementCount x))

instance AchievementCountLens ActivitySummary Integer where
  achievementCount f x = fmap
    (\ y -> x { activitySummary_achievementCount = y })
    (f (activitySummary_achievementCount x))

instance ActivityIdLens ActivityLapSummary Integer where
  activityId f x = fmap
    (\ y -> x { activityLapSummary_activityId = y })
    (f (activityLapSummary_activityId x))

instance ActivityIdLens CommentSummary Integer where
  activityId f x = fmap
    (\ y -> x { commentSummary_activityId = y })
    (f (commentSummary_activityId x))

instance ActivityIdLens EffortDetailed Integer where
  activityId f x = fmap
    (\ y -> x { effortDetailed_activityId = y })
    (f (effortDetailed_activityId x))

instance ActivityIdLens PhotoSummary Integer where
  activityId f x = fmap
    (\ y -> x { photoSummary_activityId = y })
    (f (photoSummary_activityId x))

instance ActivityIdLens SegmentLeaderboardEntry Integer where
  activityId f x = fmap
    (\ y -> x { segmentLeaderboardEntry_activityId = y })
    (f (segmentLeaderboardEntry_activityId x))

instance ActivityIdLens UploadStatus (Maybe Integer) where
  activityId f x = fmap
    (\ y -> x { uploadStatus_activityId = y })
    (f (uploadStatus_activityId x))

instance ActivityTypeLens ExploreSegmentsOptions SegmentActivityType where
  activityType f x = fmap
    (\ y -> x { exploreSegmentsOptions_activityType = y })
    (f (exploreSegmentsOptions_activityType x))

instance ActivityTypeLens SegmentDetailed ActivityType where
  activityType f x = fmap
    (\ y -> x { segmentDetailed_activityType = y })
    (f (segmentDetailed_activityType x))

instance ActivityTypeLens SegmentSummary ActivityType where
  activityType f x = fmap
    (\ y -> x { segmentSummary_activityType = y })
    (f (segmentSummary_activityType x))

instance ActivityTypeLens UploadActivityOptions (Maybe ActivityType) where
  activityType f x = fmap
    (\ y -> x { uploadActivityOptions_activityType = y })
    (f (uploadActivityOptions_activityType x))

instance AfterLens GetCurrentActivitiesOptions (Maybe UTCTime) where
  after f x = fmap
    (\ y -> x { getCurrentActivitiesOptions_after = y })
    (f (getCurrentActivitiesOptions_after x))

instance AgeGroupLens GetSegmentLeaderboardOptions (Maybe AgeGroup) where
  ageGroup f x = fmap
    (\ y -> x { getSegmentLeaderboardOptions_ageGroup = y })
    (f (getSegmentLeaderboardOptions_ageGroup x))

instance AllEffortsLens GetActivityOptions Bool where
  allEfforts f x = fmap
    (\ y -> x { getActivityOptions_allEfforts = y })
    (f (getActivityOptions_allEfforts x))

instance ApprovalPromptLens BuildAuthorizeUrlOptions Bool where
  approvalPrompt f x = fmap
    (\ y -> x { buildAuthorizeUrlOptions_approvalPrompt = y })
    (f (buildAuthorizeUrlOptions_approvalPrompt x))

instance AthleteCountLens ActivityDetailed Integer where
  athleteCount f x = fmap
    (\ y -> x { activityDetailed_athleteCount = y })
    (f (activityDetailed_athleteCount x))

instance AthleteCountLens ActivitySummary Integer where
  athleteCount f x = fmap
    (\ y -> x { activitySummary_athleteCount = y })
    (f (activitySummary_athleteCount x))

instance AthleteCountLens SegmentDetailed Integer where
  athleteCount f x = fmap
    (\ y -> x { segmentDetailed_athleteCount = y })
    (f (segmentDetailed_athleteCount x))

instance AthleteGenderLens SegmentLeaderboardEntry (Maybe Gender) where
  athleteGender f x = fmap
    (\ y -> x { segmentLeaderboardEntry_athleteGender = y })
    (f (segmentLeaderboardEntry_athleteGender x))

instance AthleteIdLens ActivityLapSummary Integer where
  athleteId f x = fmap
    (\ y -> x { activityLapSummary_athleteId = y })
    (f (activityLapSummary_athleteId x))

instance AthleteIdLens EffortDetailed Integer where
  athleteId f x = fmap
    (\ y -> x { effortDetailed_athleteId = y })
    (f (effortDetailed_athleteId x))

instance AthleteIdLens GetSegmentEffortsOptions (Maybe Integer) where
  athleteId f x = fmap
    (\ y -> x { getSegmentEffortsOptions_athleteId = y })
    (f (getSegmentEffortsOptions_athleteId x))

instance AthleteIdLens SegmentLeaderboardEntry Integer where
  athleteId f x = fmap
    (\ y -> x { segmentLeaderboardEntry_athleteId = y })
    (f (segmentLeaderboardEntry_athleteId x))

instance AthleteLens ActivityDetailed AthleteMeta where
  athlete f x = fmap
    (\ y -> x { activityDetailed_athlete = y })
    (f (activityDetailed_athlete x))

instance AthleteLens ActivitySummary AthleteMeta where
  athlete f x = fmap
    (\ y -> x { activitySummary_athlete = y })
    (f (activitySummary_athlete x))

instance AthleteLens CommentSummary AthleteSummary where
  athlete f x = fmap
    (\ y -> x { commentSummary_athlete = y })
    (f (commentSummary_athlete x))

instance AthleteLens TokenExchangeResponse AthleteDetailed where
  athlete f x = fmap
    (\ y -> x { tokenExchangeResponse_athlete = y })
    (f (tokenExchangeResponse_athlete x))

instance AthleteNameLens SegmentLeaderboardEntry Text where
  athleteName f x = fmap
    (\ y -> x { segmentLeaderboardEntry_athleteName = y })
    (f (segmentLeaderboardEntry_athleteName x))

instance AthleteProfileLens SegmentLeaderboardEntry Text where
  athleteProfile f x = fmap
    (\ y -> x { segmentLeaderboardEntry_athleteProfile = y })
    (f (segmentLeaderboardEntry_athleteProfile x))

instance AverageCadenceLens EffortDetailed (Maybe Double) where
  averageCadence f x = fmap
    (\ y -> x { effortDetailed_averageCadence = y })
    (f (effortDetailed_averageCadence x))

instance AverageGradeLens SegmentDetailed Double where
  averageGrade f x = fmap
    (\ y -> x { segmentDetailed_averageGrade = y })
    (f (segmentDetailed_averageGrade x))

instance AverageGradeLens SegmentSummary Double where
  averageGrade f x = fmap
    (\ y -> x { segmentSummary_averageGrade = y })
    (f (segmentSummary_averageGrade x))

instance AverageHeartrateLens EffortDetailed (Maybe Double) where
  averageHeartrate f x = fmap
    (\ y -> x { effortDetailed_averageHeartrate = y })
    (f (effortDetailed_averageHeartrate x))

instance AverageHrLens SegmentLeaderboardEntry Double where
  averageHr f x = fmap
    (\ y -> x { segmentLeaderboardEntry_averageHr = y })
    (f (segmentLeaderboardEntry_averageHr x))

instance AverageSpeedLens ActivityDetailed Double where
  averageSpeed f x = fmap
    (\ y -> x { activityDetailed_averageSpeed = y })
    (f (activityDetailed_averageSpeed x))

instance AverageSpeedLens ActivityLapSummary Double where
  averageSpeed f x = fmap
    (\ y -> x { activityLapSummary_averageSpeed = y })
    (f (activityLapSummary_averageSpeed x))

instance AverageSpeedLens ActivitySummary Double where
  averageSpeed f x = fmap
    (\ y -> x { activitySummary_averageSpeed = y })
    (f (activitySummary_averageSpeed x))

instance AverageWattsLens ActivityDetailed (Maybe Double) where
  averageWatts f x = fmap
    (\ y -> x { activityDetailed_averageWatts = y })
    (f (activityDetailed_averageWatts x))

instance AverageWattsLens ActivityLapSummary Double where
  averageWatts f x = fmap
    (\ y -> x { activityLapSummary_averageWatts = y })
    (f (activityLapSummary_averageWatts x))

instance AverageWattsLens ActivitySummary (Maybe Double) where
  averageWatts f x = fmap
    (\ y -> x { activitySummary_averageWatts = y })
    (f (activitySummary_averageWatts x))

instance AverageWattsLens EffortDetailed (Maybe Double) where
  averageWatts f x = fmap
    (\ y -> x { effortDetailed_averageWatts = y })
    (f (effortDetailed_averageWatts x))

instance AverageWattsLens SegmentLeaderboardEntry Double where
  averageWatts f x = fmap
    (\ y -> x { segmentLeaderboardEntry_averageWatts = y })
    (f (segmentLeaderboardEntry_averageWatts x))

instance AvgGradeLens SegmentExplorerEntry Double where
  avgGrade f x = fmap
    (\ y -> x { segmentExplorerEntry_avgGrade = y })
    (f (segmentExplorerEntry_avgGrade x))

instance BeforeLens GetCurrentActivitiesOptions (Maybe UTCTime) where
  before f x = fmap
    (\ y -> x { getCurrentActivitiesOptions_before = y })
    (f (getCurrentActivitiesOptions_before x))

instance BikesLens AthleteDetailed [GearSummary] where
  bikes f x = fmap
    (\ y -> x { athleteDetailed_bikes = y })
    (f (athleteDetailed_bikes x))

instance BrandNameLens GearDetailed Text where
  brandName f x = fmap
    (\ y -> x { gearDetailed_brandName = y })
    (f (gearDetailed_brandName x))

instance CaloriesLens ActivityDetailed Double where
  calories f x = fmap
    (\ y -> x { activityDetailed_calories = y })
    (f (activityDetailed_calories x))

instance CaptionLens PhotoSummary Text where
  caption f x = fmap
    (\ y -> x { photoSummary_caption = y })
    (f (photoSummary_caption x))

instance CityLens AthleteDetailed Text where
  city f x = fmap
    (\ y -> x { athleteDetailed_city = y })
    (f (athleteDetailed_city x))

instance CityLens AthleteSummary (Maybe Text) where
  city f x = fmap
    (\ y -> x { athleteSummary_city = y })
    (f (athleteSummary_city x))

instance CityLens ClubDetailed Text where
  city f x = fmap
    (\ y -> x { clubDetailed_city = y })
    (f (clubDetailed_city x))

instance CityLens SegmentDetailed Text where
  city f x = fmap
    (\ y -> x { segmentDetailed_city = y })
    (f (segmentDetailed_city x))

instance CityLens SegmentSummary Text where
  city f x = fmap
    (\ y -> x { segmentSummary_city = y })
    (f (segmentSummary_city x))

instance CityLens UpdateCurrentAthleteOptions (Maybe String) where
  city f x = fmap
    (\ y -> x { updateCurrentAthleteOptions_city = y })
    (f (updateCurrentAthleteOptions_city x))

instance ClimbCategoryDescLens SegmentExplorerEntry String where
  climbCategoryDesc f x = fmap
    (\ y -> x { segmentExplorerEntry_climbCategoryDesc = y })
    (f (segmentExplorerEntry_climbCategoryDesc x))

instance ClimbCategoryLens SegmentDetailed Integer where
  climbCategory f x = fmap
    (\ y -> x { segmentDetailed_climbCategory = y })
    (f (segmentDetailed_climbCategory x))

instance ClimbCategoryLens SegmentExplorerEntry Integer where
  climbCategory f x = fmap
    (\ y -> x { segmentExplorerEntry_climbCategory = y })
    (f (segmentExplorerEntry_climbCategory x))

instance ClimbCategoryLens SegmentSummary Integer where
  climbCategory f x = fmap
    (\ y -> x { segmentSummary_climbCategory = y })
    (f (segmentSummary_climbCategory x))

instance ClubIdLens GetSegmentLeaderboardOptions (Maybe Integer) where
  clubId f x = fmap
    (\ y -> x { getSegmentLeaderboardOptions_clubId = y })
    (f (getSegmentLeaderboardOptions_clubId x))

instance ClubTypeLens ClubDetailed ClubType where
  clubType f x = fmap
    (\ y -> x { clubDetailed_clubType = y })
    (f (clubDetailed_clubType x))

instance ClubsLens AthleteDetailed [ClubSummary] where
  clubs f x = fmap
    (\ y -> x { athleteDetailed_clubs = y })
    (f (athleteDetailed_clubs x))

instance CommentCountLens ActivityDetailed Integer where
  commentCount f x = fmap
    (\ y -> x { activityDetailed_commentCount = y })
    (f (activityDetailed_commentCount x))

instance CommentCountLens ActivitySummary Integer where
  commentCount f x = fmap
    (\ y -> x { activitySummary_commentCount = y })
    (f (activitySummary_commentCount x))

instance CommuteLens ActivityDetailed Bool where
  commute f x = fmap
    (\ y -> x { activityDetailed_commute = y })
    (f (activityDetailed_commute x))

instance CommuteLens ActivitySummary Bool where
  commute f x = fmap
    (\ y -> x { activitySummary_commute = y })
    (f (activitySummary_commute x))

instance CommuteLens UpdateActivityOptions (Maybe Bool) where
  commute f x = fmap
    (\ y -> x { updateActivityOptions_commute = y })
    (f (updateActivityOptions_commute x))

instance CountryLens AthleteDetailed Text where
  country f x = fmap
    (\ y -> x { athleteDetailed_country = y })
    (f (athleteDetailed_country x))

instance CountryLens AthleteSummary (Maybe Text) where
  country f x = fmap
    (\ y -> x { athleteSummary_country = y })
    (f (athleteSummary_country x))

instance CountryLens ClubDetailed Text where
  country f x = fmap
    (\ y -> x { clubDetailed_country = y })
    (f (clubDetailed_country x))

instance CountryLens SegmentDetailed Text where
  country f x = fmap
    (\ y -> x { segmentDetailed_country = y })
    (f (segmentDetailed_country x))

instance CountryLens SegmentSummary Text where
  country f x = fmap
    (\ y -> x { segmentSummary_country = y })
    (f (segmentSummary_country x))

instance CountryLens UpdateCurrentAthleteOptions (Maybe String) where
  country f x = fmap
    (\ y -> x { updateCurrentAthleteOptions_country = y })
    (f (updateCurrentAthleteOptions_country x))

instance CreatedAtLens AthleteDetailed UTCTime where
  createdAt f x = fmap
    (\ y -> x { athleteDetailed_createdAt = y })
    (f (athleteDetailed_createdAt x))

instance CreatedAtLens AthleteSummary UTCTime where
  createdAt f x = fmap
    (\ y -> x { athleteSummary_createdAt = y })
    (f (athleteSummary_createdAt x))

instance CreatedAtLens CommentSummary UTCTime where
  createdAt f x = fmap
    (\ y -> x { commentSummary_createdAt = y })
    (f (commentSummary_createdAt x))

instance CreatedAtLens PhotoSummary UTCTime where
  createdAt f x = fmap
    (\ y -> x { photoSummary_createdAt = y })
    (f (photoSummary_createdAt x))

instance CreatedAtLens SegmentDetailed UTCTime where
  createdAt f x = fmap
    (\ y -> x { segmentDetailed_createdAt = y })
    (f (segmentDetailed_createdAt x))

instance DataLens StreamDetailed [Value] where
  data_ f x = fmap
    (\ y -> x { streamDetailed_data = y })
    (f (streamDetailed_data x))

instance DatePreferenceLens AthleteDetailed Text where
  datePreference f x = fmap
    (\ y -> x { athleteDetailed_datePreference = y })
    (f (athleteDetailed_datePreference x))

instance DateRangeLens GetSegmentLeaderboardOptions (Maybe String) where
  dateRange f x = fmap
    (\ y -> x { getSegmentLeaderboardOptions_dateRange = y })
    (f (getSegmentLeaderboardOptions_dateRange x))

instance DescriptionLens ActivityDetailed (Maybe Text) where
  description f x = fmap
    (\ y -> x { activityDetailed_description = y })
    (f (activityDetailed_description x))

instance DescriptionLens ClubDetailed Text where
  description f x = fmap
    (\ y -> x { clubDetailed_description = y })
    (f (clubDetailed_description x))

instance DescriptionLens CreateActivityOptions (Maybe String) where
  description f x = fmap
    (\ y -> x { createActivityOptions_description = y })
    (f (createActivityOptions_description x))

instance DescriptionLens GearDetailed Text where
  description f x = fmap
    (\ y -> x { gearDetailed_description = y })
    (f (gearDetailed_description x))

instance DescriptionLens UpdateActivityOptions (Maybe String) where
  description f x = fmap
    (\ y -> x { updateActivityOptions_description = y })
    (f (updateActivityOptions_description x))

instance DescriptionLens UploadActivityOptions (Maybe String) where
  description f x = fmap
    (\ y -> x { uploadActivityOptions_description = y })
    (f (uploadActivityOptions_description x))

instance DistanceLens ActivityDetailed Double where
  distance f x = fmap
    (\ y -> x { activityDetailed_distance = y })
    (f (activityDetailed_distance x))

instance DistanceLens ActivityLapSummary Double where
  distance f x = fmap
    (\ y -> x { activityLapSummary_distance = y })
    (f (activityLapSummary_distance x))

instance DistanceLens ActivitySummary Double where
  distance f x = fmap
    (\ y -> x { activitySummary_distance = y })
    (f (activitySummary_distance x))

instance DistanceLens CreateActivityOptions (Maybe Double) where
  distance f x = fmap
    (\ y -> x { createActivityOptions_distance = y })
    (f (createActivityOptions_distance x))

instance DistanceLens EffortDetailed Double where
  distance f x = fmap
    (\ y -> x { effortDetailed_distance = y })
    (f (effortDetailed_distance x))

instance DistanceLens GearDetailed Double where
  distance f x = fmap
    (\ y -> x { gearDetailed_distance = y })
    (f (gearDetailed_distance x))

instance DistanceLens GearSummary Double where
  distance f x = fmap
    (\ y -> x { gearSummary_distance = y })
    (f (gearSummary_distance x))

instance DistanceLens SegmentDetailed Double where
  distance f x = fmap
    (\ y -> x { segmentDetailed_distance = y })
    (f (segmentDetailed_distance x))

instance DistanceLens SegmentExplorerEntry Double where
  distance f x = fmap
    (\ y -> x { segmentExplorerEntry_distance = y })
    (f (segmentExplorerEntry_distance x))

instance DistanceLens SegmentLeaderboardEntry Double where
  distance f x = fmap
    (\ y -> x { segmentLeaderboardEntry_distance = y })
    (f (segmentLeaderboardEntry_distance x))

instance DistanceLens SegmentSummary Double where
  distance f x = fmap
    (\ y -> x { segmentSummary_distance = y })
    (f (segmentSummary_distance x))

instance DistributionBucketsLens ActivityZoneDetailed [ActivityZoneDistributionBucket] where
  distributionBuckets f x = fmap
    (\ y -> x { activityZoneDetailed_distributionBuckets = y })
    (f (activityZoneDetailed_distributionBuckets x))

instance EffortCountLens SegmentDetailed Integer where
  effortCount f x = fmap
    (\ y -> x { segmentDetailed_effortCount = y })
    (f (segmentDetailed_effortCount x))

instance EffortCountLens SegmentLeaderboardResponse Integer where
  effortCount f x = fmap
    (\ y -> x { segmentLeaderboard_effortCount = y })
    (f (segmentLeaderboard_effortCount x))

instance EffortIdLens SegmentLeaderboardEntry Integer where
  effortId f x = fmap
    (\ y -> x { segmentLeaderboardEntry_effortId = y })
    (f (segmentLeaderboardEntry_effortId x))

instance ElapsedTimeLens ActivityDetailed Integer where
  elapsedTime f x = fmap
    (\ y -> x { activityDetailed_elapsedTime = y })
    (f (activityDetailed_elapsedTime x))

instance ElapsedTimeLens ActivityLapSummary Integer where
  elapsedTime f x = fmap
    (\ y -> x { activityLapSummary_elapsedTime = y })
    (f (activityLapSummary_elapsedTime x))

instance ElapsedTimeLens ActivitySummary Integer where
  elapsedTime f x = fmap
    (\ y -> x { activitySummary_elapsedTime = y })
    (f (activitySummary_elapsedTime x))

instance ElapsedTimeLens EffortDetailed Integer where
  elapsedTime f x = fmap
    (\ y -> x { effortDetailed_elapsedTime = y })
    (f (effortDetailed_elapsedTime x))

instance ElapsedTimeLens SegmentLeaderboardEntry Integer where
  elapsedTime f x = fmap
    (\ y -> x { segmentLeaderboardEntry_elapsedTime = y })
    (f (segmentLeaderboardEntry_elapsedTime x))

instance ElevDifferenceLens SegmentExplorerEntry Double where
  elevDifference f x = fmap
    (\ y -> x { segmentExplorerEntry_elevDifference = y })
    (f (segmentExplorerEntry_elevDifference x))

instance ElevationHighLens SegmentDetailed Double where
  elevationHigh f x = fmap
    (\ y -> x { segmentDetailed_elevationHigh = y })
    (f (segmentDetailed_elevationHigh x))

instance ElevationHighLens SegmentSummary Double where
  elevationHigh f x = fmap
    (\ y -> x { segmentSummary_elevationHigh = y })
    (f (segmentSummary_elevationHigh x))

instance ElevationLowLens SegmentDetailed Double where
  elevationLow f x = fmap
    (\ y -> x { segmentDetailed_elevationLow = y })
    (f (segmentDetailed_elevationLow x))

instance ElevationLowLens SegmentSummary Double where
  elevationLow f x = fmap
    (\ y -> x { segmentSummary_elevationLow = y })
    (f (segmentSummary_elevationLow x))

instance EmailLens AthleteDetailed Text where
  email f x = fmap
    (\ y -> x { athleteDetailed_email = y })
    (f (athleteDetailed_email x))

instance EndIndexLens ActivityLapSummary Integer where
  endIndex f x = fmap
    (\ y -> x { activityLapSummary_endIndex = y })
    (f (activityLapSummary_endIndex x))

instance EndIndexLens EffortDetailed Integer where
  endIndex f x = fmap
    (\ y -> x { effortDetailed_endIndex = y })
    (f (effortDetailed_endIndex x))

instance EndLatitudeLens SegmentDetailed Double where
  endLatitude f x = fmap
    (\ y -> x { segmentDetailed_endLatitude = y })
    (f (segmentDetailed_endLatitude x))

instance EndLatitudeLens SegmentSummary Double where
  endLatitude f x = fmap
    (\ y -> x { segmentSummary_endLatitude = y })
    (f (segmentSummary_endLatitude x))

instance EndLatlngLens ActivityDetailed (Maybe (Double, Double)) where
  endLatlng f x = fmap
    (\ y -> x { activityDetailed_endLatlng = y })
    (f (activityDetailed_endLatlng x))

instance EndLatlngLens ActivitySummary (Maybe (Double, Double)) where
  endLatlng f x = fmap
    (\ y -> x { activitySummary_endLatlng = y })
    (f (activitySummary_endLatlng x))

instance EndLatlngLens SegmentDetailed ((Double, Double)) where
  endLatlng f x = fmap
    (\ y -> x { segmentDetailed_endLatlng = y })
    (f (segmentDetailed_endLatlng x))

instance EndLatlngLens SegmentExplorerEntry ((Double, Double)) where
  endLatlng f x = fmap
    (\ y -> x { segmentExplorerEntry_endLatlng = y })
    (f (segmentExplorerEntry_endLatlng x))

instance EndLatlngLens SegmentSummary ((Double, Double)) where
  endLatlng f x = fmap
    (\ y -> x { segmentSummary_endLatlng = y })
    (f (segmentSummary_endLatlng x))

instance EndLongitudeLens SegmentDetailed Double where
  endLongitude f x = fmap
    (\ y -> x { segmentDetailed_endLongitude = y })
    (f (segmentDetailed_endLongitude x))

instance EndLongitudeLens SegmentSummary Double where
  endLongitude f x = fmap
    (\ y -> x { segmentSummary_endLongitude = y })
    (f (segmentSummary_endLongitude x))

instance EntriesLens SegmentExplorerResponse [SegmentExplorerEntry] where
  entries f x = fmap
    (\ y -> x { segmentExplorerResponse_entries = y })
    (f (segmentExplorerResponse_entries x))

instance EntriesLens SegmentLeaderboardResponse [SegmentLeaderboardEntry] where
  entries f x = fmap
    (\ y -> x { segmentLeaderboard_entries = y })
    (f (segmentLeaderboard_entries x))

instance EntryCountLens SegmentLeaderboardResponse Integer where
  entryCount f x = fmap
    (\ y -> x { segmentLeaderboard_entryCount = y })
    (f (segmentLeaderboard_entryCount x))

instance ErrorLens UploadStatus (Maybe Text) where
  error_ f x = fmap
    (\ y -> x { uploadStatus_error = y })
    (f (uploadStatus_error x))

instance ExternalIdLens ActivityDetailed (Maybe Text) where
  externalId f x = fmap
    (\ y -> x { activityDetailed_externalId = y })
    (f (activityDetailed_externalId x))

instance ExternalIdLens ActivitySummary (Maybe Text) where
  externalId f x = fmap
    (\ y -> x { activitySummary_externalId = y })
    (f (activitySummary_externalId x))

instance ExternalIdLens UploadActivityOptions (Maybe String) where
  externalId f x = fmap
    (\ y -> x { uploadActivityOptions_externalId = y })
    (f (uploadActivityOptions_externalId x))

instance ExternalIdLens UploadStatus (Maybe Text) where
  externalId f x = fmap
    (\ y -> x { uploadStatus_externalId = y })
    (f (uploadStatus_externalId x))

instance FirstnameLens AthleteDetailed Text where
  firstname f x = fmap
    (\ y -> x { athleteDetailed_firstname = y })
    (f (athleteDetailed_firstname x))

instance FirstnameLens AthleteSummary Text where
  firstname f x = fmap
    (\ y -> x { athleteSummary_firstname = y })
    (f (athleteSummary_firstname x))

instance FlaggedLens ActivityDetailed Bool where
  flagged f x = fmap
    (\ y -> x { activityDetailed_flagged = y })
    (f (activityDetailed_flagged x))

instance FlaggedLens ActivitySummary Bool where
  flagged f x = fmap
    (\ y -> x { activitySummary_flagged = y })
    (f (activitySummary_flagged x))

instance FollowerCountLens AthleteDetailed Integer where
  followerCount f x = fmap
    (\ y -> x { athleteDetailed_followerCount = y })
    (f (athleteDetailed_followerCount x))

instance FollowerLens AthleteDetailed (Maybe Text) where
  follower f x = fmap
    (\ y -> x { athleteDetailed_follower = y })
    (f (athleteDetailed_follower x))

instance FollowerLens AthleteSummary (Maybe Text) where
  follower f x = fmap
    (\ y -> x { athleteSummary_follower = y })
    (f (athleteSummary_follower x))

instance FollowingLens GetSegmentLeaderboardOptions (Maybe Bool) where
  following f x = fmap
    (\ y -> x { getSegmentLeaderboardOptions_following = y })
    (f (getSegmentLeaderboardOptions_following x))

instance FrameTypeLens GearDetailed (Maybe FrameType) where
  frameType f x = fmap
    (\ y -> x { gearDetailed_frameType = y })
    (f (gearDetailed_frameType x))

instance FriendCountLens AthleteDetailed Integer where
  friendCount f x = fmap
    (\ y -> x { athleteDetailed_friendCount = y })
    (f (athleteDetailed_friendCount x))

instance FriendLens AthleteDetailed (Maybe Text) where
  friend f x = fmap
    (\ y -> x { athleteDetailed_friend = y })
    (f (athleteDetailed_friend x))

instance FriendLens AthleteSummary (Maybe Text) where
  friend f x = fmap
    (\ y -> x { athleteSummary_friend = y })
    (f (athleteSummary_friend x))

instance FtpLens AthleteDetailed (Maybe Integer) where
  ftp f x = fmap
    (\ y -> x { athleteDetailed_ftp = y })
    (f (athleteDetailed_ftp x))

instance GearIdLens ActivityDetailed (Maybe Text) where
  gearId f x = fmap
    (\ y -> x { activityDetailed_gearId = y })
    (f (activityDetailed_gearId x))

instance GearIdLens ActivitySummary (Maybe Text) where
  gearId f x = fmap
    (\ y -> x { activitySummary_gearId = y })
    (f (activitySummary_gearId x))

instance GearIdLens UpdateActivityOptions (Maybe String) where
  gearId f x = fmap
    (\ y -> x { updateActivityOptions_gearId = y })
    (f (updateActivityOptions_gearId x))

instance GearLens ActivityDetailed GearSummary where
  gear f x = fmap
    (\ y -> x { activityDetailed_gear = y })
    (f (activityDetailed_gear x))

instance GenderLens GetSegmentLeaderboardOptions (Maybe Gender) where
  gender f x = fmap
    (\ y -> x { getSegmentLeaderboardOptions_gender = y })
    (f (getSegmentLeaderboardOptions_gender x))

instance HasKudoedLens ActivityDetailed Bool where
  hasKudoed f x = fmap
    (\ y -> x { activityDetailed_hasKudoed = y })
    (f (activityDetailed_hasKudoed x))

instance HasKudoedLens ActivitySummary Bool where
  hasKudoed f x = fmap
    (\ y -> x { activitySummary_hasKudoed = y })
    (f (activitySummary_hasKudoed x))

instance HazardousLens SegmentDetailed Bool where
  hazardous f x = fmap
    (\ y -> x { segmentDetailed_hazardous = y })
    (f (segmentDetailed_hazardous x))

instance HiddenLens EffortDetailed (Maybe Bool) where
  hidden f x = fmap
    (\ y -> x { effortDetailed_hidden = y })
    (f (effortDetailed_hidden x))

instance IdLens ActivityDetailed Integer where
  id_ f x = fmap
    (\ y -> x { activityDetailed_id = y })
    (f (activityDetailed_id x))

instance IdLens ActivityLapSummary Integer where
  id_ f x = fmap
    (\ y -> x { activityLapSummary_id = y })
    (f (activityLapSummary_id x))

instance IdLens ActivitySummary Integer where
  id_ f x = fmap
    (\ y -> x { activitySummary_id = y })
    (f (activitySummary_id x))

instance IdLens AthleteDetailed Integer where
  id_ f x = fmap
    (\ y -> x { athleteDetailed_id = y })
    (f (athleteDetailed_id x))

instance IdLens AthleteMeta Integer where
  id_ f x = fmap
    (\ y -> x { athleteMeta_id = y })
    (f (athleteMeta_id x))

instance IdLens AthleteSummary Integer where
  id_ f x = fmap
    (\ y -> x { athleteSummary_id = y })
    (f (athleteSummary_id x))

instance IdLens ClubDetailed Integer where
  id_ f x = fmap
    (\ y -> x { clubDetailed_id = y })
    (f (clubDetailed_id x))

instance IdLens ClubSummary Integer where
  id_ f x = fmap
    (\ y -> x { clubSummary_id = y })
    (f (clubSummary_id x))

instance IdLens CommentSummary Integer where
  id_ f x = fmap
    (\ y -> x { commentSummary_id = y })
    (f (commentSummary_id x))

instance IdLens EffortDetailed Integer where
  id_ f x = fmap
    (\ y -> x { effortDetailed_id = y })
    (f (effortDetailed_id x))

instance IdLens GearDetailed Text where
  id_ f x = fmap
    (\ y -> x { gearDetailed_id = y })
    (f (gearDetailed_id x))

instance IdLens GearSummary Text where
  id_ f x = fmap
    (\ y -> x { gearSummary_id = y })
    (f (gearSummary_id x))

instance IdLens PhotoSummary Integer where
  id_ f x = fmap
    (\ y -> x { photoSummary_id = y })
    (f (photoSummary_id x))

instance IdLens PolylineDetailed Text where
  id_ f x = fmap
    (\ y -> x { polylineDetailed_id = y })
    (f (polylineDetailed_id x))

instance IdLens PolylineSummary Text where
  id_ f x = fmap
    (\ y -> x { polylineSummary_id = y })
    (f (polylineSummary_id x))

instance IdLens SegmentDetailed Integer where
  id_ f x = fmap
    (\ y -> x { segmentDetailed_id = y })
    (f (segmentDetailed_id x))

instance IdLens SegmentExplorerEntry Integer where
  id_ f x = fmap
    (\ y -> x { segmentExplorerEntry_id = y })
    (f (segmentExplorerEntry_id x))

instance IdLens SegmentSummary Integer where
  id_ f x = fmap
    (\ y -> x { segmentSummary_id = y })
    (f (segmentSummary_id x))

instance IdLens UploadStatus Integer where
  id_ f x = fmap
    (\ y -> x { uploadStatus_id = y })
    (f (uploadStatus_id x))

instance InstagramPrimaryPhotoLens ActivityDetailed (Maybe Text) where
  instagramPrimaryPhoto f x = fmap
    (\ y -> x { activityDetailed_instagramPrimaryPhoto = y })
    (f (activityDetailed_instagramPrimaryPhoto x))

instance KilojoulesLens ActivityDetailed (Maybe Double) where
  kilojoules f x = fmap
    (\ y -> x { activityDetailed_kilojoules = y })
    (f (activityDetailed_kilojoules x))

instance KilojoulesLens ActivitySummary (Maybe Double) where
  kilojoules f x = fmap
    (\ y -> x { activitySummary_kilojoules = y })
    (f (activitySummary_kilojoules x))

instance KomRankLens EffortDetailed (Maybe Integer) where
  komRank f x = fmap
    (\ y -> x { effortDetailed_komRank = y })
    (f (effortDetailed_komRank x))

instance KudosCountLens ActivitySummary Integer where
  kudosCount f x = fmap
    (\ y -> x { activitySummary_kudosCount = y })
    (f (activitySummary_kudosCount x))

instance LapIndexLens ActivityLapSummary Integer where
  lapIndex f x = fmap
    (\ y -> x { activityLapSummary_lapIndex = y })
    (f (activityLapSummary_lapIndex x))

instance LastnameLens AthleteDetailed Text where
  lastname f x = fmap
    (\ y -> x { athleteDetailed_lastname = y })
    (f (athleteDetailed_lastname x))

instance LastnameLens AthleteSummary Text where
  lastname f x = fmap
    (\ y -> x { athleteSummary_lastname = y })
    (f (athleteSummary_lastname x))

instance LocationCityLens ActivityDetailed (Maybe Text) where
  locationCity f x = fmap
    (\ y -> x { activityDetailed_locationCity = y })
    (f (activityDetailed_locationCity x))

instance LocationCityLens ActivitySummary (Maybe Text) where
  locationCity f x = fmap
    (\ y -> x { activitySummary_locationCity = y })
    (f (activitySummary_locationCity x))

instance LocationCountryLens ActivityDetailed Text where
  locationCountry f x = fmap
    (\ y -> x { activityDetailed_locationCountry = y })
    (f (activityDetailed_locationCountry x))

instance LocationCountryLens ActivitySummary Text where
  locationCountry f x = fmap
    (\ y -> x { activitySummary_locationCountry = y })
    (f (activitySummary_locationCountry x))

instance LocationLens PhotoSummary (Maybe (Double, Double)) where
  location f x = fmap
    (\ y -> x { photoSummary_location = y })
    (f (photoSummary_location x))

instance LocationStateLens ActivityDetailed (Maybe Text) where
  locationState f x = fmap
    (\ y -> x { activityDetailed_locationState = y })
    (f (activityDetailed_locationState x))

instance LocationStateLens ActivitySummary (Maybe Text) where
  locationState f x = fmap
    (\ y -> x { activitySummary_locationState = y })
    (f (activitySummary_locationState x))

instance ManualLens ActivityDetailed Bool where
  manual f x = fmap
    (\ y -> x { activityDetailed_manual = y })
    (f (activityDetailed_manual x))

instance ManualLens ActivitySummary Bool where
  manual f x = fmap
    (\ y -> x { activitySummary_manual = y })
    (f (activitySummary_manual x))

instance MapLens ActivityDetailed PolylineDetailed where
  map f x = fmap
    (\ y -> x { activityDetailed_map = y })
    (f (activityDetailed_map x))

instance MapLens ActivitySummary PolylineSummary where
  map f x = fmap
    (\ y -> x { activitySummary_map = y })
    (f (activitySummary_map x))

instance MapLens SegmentDetailed PolylineDetailed where
  map f x = fmap
    (\ y -> x { segmentDetailed_map = y })
    (f (segmentDetailed_map x))

instance MarkdownLens GetActivityCommentsOptions Bool where
  markdown f x = fmap
    (\ y -> x { getActivityCommentsOptions_markdown = y })
    (f (getActivityCommentsOptions_markdown x))

instance MaxCatLens ExploreSegmentsOptions Integer where
  maxCat f x = fmap
    (\ y -> x { exploreSegmentsOptions_maxCat = y })
    (f (exploreSegmentsOptions_maxCat x))

instance MaxHeartrateLens EffortDetailed (Maybe Integer) where
  maxHeartrate f x = fmap
    (\ y -> x { effortDetailed_maxHeartrate = y })
    (f (effortDetailed_maxHeartrate x))

instance MaxLens ActivityZoneDistributionBucket Integer where
  max_ f x = fmap
    (\ y -> x { activityZoneDistributionBucket_max = y })
    (f (activityZoneDistributionBucket_max x))

instance MaxSpeedLens ActivityDetailed Double where
  maxSpeed f x = fmap
    (\ y -> x { activityDetailed_maxSpeed = y })
    (f (activityDetailed_maxSpeed x))

instance MaxSpeedLens ActivityLapSummary Double where
  maxSpeed f x = fmap
    (\ y -> x { activityLapSummary_maxSpeed = y })
    (f (activityLapSummary_maxSpeed x))

instance MaxSpeedLens ActivitySummary Double where
  maxSpeed f x = fmap
    (\ y -> x { activitySummary_maxSpeed = y })
    (f (activitySummary_maxSpeed x))

instance MaximumGradeLens SegmentDetailed Double where
  maximumGrade f x = fmap
    (\ y -> x { segmentDetailed_maximumGrade = y })
    (f (segmentDetailed_maximumGrade x))

instance MaximumGradeLens SegmentSummary Double where
  maximumGrade f x = fmap
    (\ y -> x { segmentSummary_maximumGrade = y })
    (f (segmentSummary_maximumGrade x))

instance MeasurementPreferenceLens AthleteDetailed MeasurementPreference where
  measurementPreference f x = fmap
    (\ y -> x { athleteDetailed_measurementPreference = y })
    (f (athleteDetailed_measurementPreference x))

instance MemberCountLens ClubDetailed Integer where
  memberCount f x = fmap
    (\ y -> x { clubDetailed_memberCount = y })
    (f (clubDetailed_memberCount x))

instance MinCatLens ExploreSegmentsOptions Integer where
  minCat f x = fmap
    (\ y -> x { exploreSegmentsOptions_minCat = y })
    (f (exploreSegmentsOptions_minCat x))

instance MinLens ActivityZoneDistributionBucket Integer where
  min_ f x = fmap
    (\ y -> x { activityZoneDistributionBucket_min = y })
    (f (activityZoneDistributionBucket_min x))

instance ModelNameLens GearDetailed Text where
  modelName f x = fmap
    (\ y -> x { gearDetailed_modelName = y })
    (f (gearDetailed_modelName x))

instance MovingTimeLens ActivityDetailed Integer where
  movingTime f x = fmap
    (\ y -> x { activityDetailed_movingTime = y })
    (f (activityDetailed_movingTime x))

instance MovingTimeLens ActivityLapSummary Double where
  movingTime f x = fmap
    (\ y -> x { activityLapSummary_movingTime = y })
    (f (activityLapSummary_movingTime x))

instance MovingTimeLens ActivitySummary Integer where
  movingTime f x = fmap
    (\ y -> x { activitySummary_movingTime = y })
    (f (activitySummary_movingTime x))

instance MovingTimeLens EffortDetailed Integer where
  movingTime f x = fmap
    (\ y -> x { effortDetailed_movingTime = y })
    (f (effortDetailed_movingTime x))

instance MovingTimeLens SegmentLeaderboardEntry Integer where
  movingTime f x = fmap
    (\ y -> x { segmentLeaderboardEntry_movingTime = y })
    (f (segmentLeaderboardEntry_movingTime x))

instance MutualFriendCountLens AthleteDetailed Integer where
  mutualFriendCount f x = fmap
    (\ y -> x { athleteDetailed_mutualFriendCount = y })
    (f (athleteDetailed_mutualFriendCount x))

instance NameLens ActivityDetailed Text where
  name f x = fmap
    (\ y -> x { activityDetailed_name = y })
    (f (activityDetailed_name x))

instance NameLens ActivityLapSummary Text where
  name f x = fmap
    (\ y -> x { activityLapSummary_name = y })
    (f (activityLapSummary_name x))

instance NameLens ActivitySummary Text where
  name f x = fmap
    (\ y -> x { activitySummary_name = y })
    (f (activitySummary_name x))

instance NameLens ClubDetailed Text where
  name f x = fmap
    (\ y -> x { clubDetailed_name = y })
    (f (clubDetailed_name x))

instance NameLens ClubSummary Text where
  name f x = fmap
    (\ y -> x { clubSummary_name = y })
    (f (clubSummary_name x))

instance NameLens EffortDetailed Text where
  name f x = fmap
    (\ y -> x { effortDetailed_name = y })
    (f (effortDetailed_name x))

instance NameLens GearDetailed Text where
  name f x = fmap
    (\ y -> x { gearDetailed_name = y })
    (f (gearDetailed_name x))

instance NameLens GearSummary Text where
  name f x = fmap
    (\ y -> x { gearSummary_name = y })
    (f (gearSummary_name x))

instance NameLens SegmentDetailed Text where
  name f x = fmap
    (\ y -> x { segmentDetailed_name = y })
    (f (segmentDetailed_name x))

instance NameLens SegmentExplorerEntry Text where
  name f x = fmap
    (\ y -> x { segmentExplorerEntry_name = y })
    (f (segmentExplorerEntry_name x))

instance NameLens SegmentSummary Text where
  name f x = fmap
    (\ y -> x { segmentSummary_name = y })
    (f (segmentSummary_name x))

instance NameLens UpdateActivityOptions (Maybe String) where
  name f x = fmap
    (\ y -> x { updateActivityOptions_name = y })
    (f (updateActivityOptions_name x))

instance NameLens UploadActivityOptions (Maybe String) where
  name f x = fmap
    (\ y -> x { uploadActivityOptions_name = y })
    (f (uploadActivityOptions_name x))

instance OriginalSizeLens StreamDetailed Integer where
  originalSize f x = fmap
    (\ y -> x { streamDetailed_originalSize = y })
    (f (streamDetailed_originalSize x))

instance PageLens GetActivityCommentsOptions Integer where
  page f x = fmap
    (\ y -> x { getActivityCommentsOptions_page = y })
    (f (getActivityCommentsOptions_page x))

instance PageLens GetCurrentActivitiesOptions Integer where
  page f x = fmap
    (\ y -> x { getCurrentActivitiesOptions_page = y })
    (f (getCurrentActivitiesOptions_page x))

instance PageLens GetSegmentEffortsOptions Integer where
  page f x = fmap
    (\ y -> x { getSegmentEffortsOptions_page = y })
    (f (getSegmentEffortsOptions_page x))

instance PageLens GetSegmentLeaderboardOptions Integer where
  page f x = fmap
    (\ y -> x { getSegmentLeaderboardOptions_page = y })
    (f (getSegmentLeaderboardOptions_page x))

instance PageLens PaginationOptions Integer where
  page f x = fmap
    (\ y -> x { paginationOptions_page = y })
    (f (paginationOptions_page x))

instance PerPageLens GetActivityCommentsOptions Integer where
  perPage f x = fmap
    (\ y -> x { getActivityCommentsOptions_perPage = y })
    (f (getActivityCommentsOptions_perPage x))

instance PerPageLens GetCurrentActivitiesOptions Integer where
  perPage f x = fmap
    (\ y -> x { getCurrentActivitiesOptions_perPage = y })
    (f (getCurrentActivitiesOptions_perPage x))

instance PerPageLens GetSegmentEffortsOptions Integer where
  perPage f x = fmap
    (\ y -> x { getSegmentEffortsOptions_perPage = y })
    (f (getSegmentEffortsOptions_perPage x))

instance PerPageLens GetSegmentLeaderboardOptions Integer where
  perPage f x = fmap
    (\ y -> x { getSegmentLeaderboardOptions_perPage = y })
    (f (getSegmentLeaderboardOptions_perPage x))

instance PerPageLens PaginationOptions Integer where
  perPage f x = fmap
    (\ y -> x { paginationOptions_perPage = y })
    (f (paginationOptions_perPage x))

instance PhotoCountLens ActivityDetailed Integer where
  photoCount f x = fmap
    (\ y -> x { activityDetailed_photoCount = y })
    (f (activityDetailed_photoCount x))

instance PhotoCountLens ActivitySummary Integer where
  photoCount f x = fmap
    (\ y -> x { activitySummary_photoCount = y })
    (f (activitySummary_photoCount x))

instance PointsLens SegmentExplorerEntry Text where
  points f x = fmap
    (\ y -> x { segmentExplorerEntry_points = y })
    (f (segmentExplorerEntry_points x))

instance PolylineLens PolylineDetailed ([(Double, Double)]) where
  polyline f x = fmap
    (\ y -> x { polylineDetailed_polyline = y })
    (f (polylineDetailed_polyline x))

instance PrRankLens EffortDetailed (Maybe Integer) where
  prRank f x = fmap
    (\ y -> x { effortDetailed_prRank = y })
    (f (effortDetailed_prRank x))

instance PremiumLens AthleteDetailed Bool where
  premium f x = fmap
    (\ y -> x { athleteDetailed_premium = y })
    (f (athleteDetailed_premium x))

instance PremiumLens AthleteSummary Bool where
  premium f x = fmap
    (\ y -> x { athleteSummary_premium = y })
    (f (athleteSummary_premium x))

instance PrimaryLens GearDetailed Bool where
  primary f x = fmap
    (\ y -> x { gearDetailed_primary = y })
    (f (gearDetailed_primary x))

instance PrimaryLens GearSummary Bool where
  primary f x = fmap
    (\ y -> x { gearSummary_primary = y })
    (f (gearSummary_primary x))

instance PrivateLens ActivityDetailed Bool where
  private f x = fmap
    (\ y -> x { activityDetailed_private = y })
    (f (activityDetailed_private x))

instance PrivateLens ActivitySummary Bool where
  private f x = fmap
    (\ y -> x { activitySummary_private = y })
    (f (activitySummary_private x))

instance PrivateLens ClubDetailed Bool where
  private f x = fmap
    (\ y -> x { clubDetailed_private = y })
    (f (clubDetailed_private x))

instance PrivateLens SegmentDetailed Bool where
  private f x = fmap
    (\ y -> x { segmentDetailed_private = y })
    (f (segmentDetailed_private x))

instance PrivateLens SegmentSummary Bool where
  private f x = fmap
    (\ y -> x { segmentSummary_private = y })
    (f (segmentSummary_private x))

instance PrivateLens UpdateActivityOptions (Maybe Bool) where
  private f x = fmap
    (\ y -> x { updateActivityOptions_private = y })
    (f (updateActivityOptions_private x))

instance PrivateLens UploadActivityOptions Bool where
  private f x = fmap
    (\ y -> x { uploadActivityOptions_private = y })
    (f (uploadActivityOptions_private x))

instance PrivateScopeLens BuildAuthorizeUrlOptions Bool where
  privateScope f x = fmap
    (\ y -> x { buildAuthorizeUrlOptions_privateScope = y })
    (f (buildAuthorizeUrlOptions_privateScope x))

instance ProfileLens AthleteDetailed Text where
  profile f x = fmap
    (\ y -> x { athleteDetailed_profile = y })
    (f (athleteDetailed_profile x))

instance ProfileLens AthleteSummary Text where
  profile f x = fmap
    (\ y -> x { athleteSummary_profile = y })
    (f (athleteSummary_profile x))

instance ProfileLens ClubDetailed Text where
  profile f x = fmap
    (\ y -> x { clubDetailed_profile = y })
    (f (clubDetailed_profile x))

instance ProfileLens ClubSummary Text where
  profile f x = fmap
    (\ y -> x { clubSummary_profile = y })
    (f (clubSummary_profile x))

instance ProfileMediumLens AthleteDetailed Text where
  profileMedium f x = fmap
    (\ y -> x { athleteDetailed_profileMedium = y })
    (f (athleteDetailed_profileMedium x))

instance ProfileMediumLens AthleteSummary Text where
  profileMedium f x = fmap
    (\ y -> x { athleteSummary_profileMedium = y })
    (f (athleteSummary_profileMedium x))

instance ProfileMediumLens ClubDetailed Text where
  profileMedium f x = fmap
    (\ y -> x { clubDetailed_profileMedium = y })
    (f (clubDetailed_profileMedium x))

instance ProfileMediumLens ClubSummary Text where
  profileMedium f x = fmap
    (\ y -> x { clubSummary_profileMedium = y })
    (f (clubSummary_profileMedium x))

instance RangeLens GetSegmentEffortsOptions (Maybe (UTCTime, UTCTime)) where
  range f x = fmap
    (\ y -> x { getSegmentEffortsOptions_range = y })
    (f (getSegmentEffortsOptions_range x))

instance RankLens SegmentLeaderboardEntry Integer where
  rank f x = fmap
    (\ y -> x { segmentLeaderboardEntry_rank = y })
    (f (segmentLeaderboardEntry_rank x))

instance RefLens PhotoSummary Text where
  ref f x = fmap
    (\ y -> x { photoSummary_ref = y })
    (f (photoSummary_ref x))

instance RequesterLens Client (Request -> IO (Response ByteString)) where
  requester f x = fmap
    (\ y -> x { client_requester = y })
    (f (client_requester x))

instance ResolutionLens GetStreamsOptions (Maybe Resolution) where
  resolution f x = fmap
    (\ y -> x { getStreamsOptions_resolution = y })
    (f (getStreamsOptions_resolution x))

instance ResolutionLens StreamDetailed Resolution where
  resolution f x = fmap
    (\ y -> x { streamDetailed_resolution = y })
    (f (streamDetailed_resolution x))

instance ResourceStateLens ActivityDetailed ResourceState where
  resourceState f x = fmap
    (\ y -> x { activityDetailed_resourceState = y })
    (f (activityDetailed_resourceState x))

instance ResourceStateLens ActivityLapSummary ResourceState where
  resourceState f x = fmap
    (\ y -> x { activityLapSummary_resourceState = y })
    (f (activityLapSummary_resourceState x))

instance ResourceStateLens ActivitySummary ResourceState where
  resourceState f x = fmap
    (\ y -> x { activitySummary_resourceState = y })
    (f (activitySummary_resourceState x))

instance ResourceStateLens ActivityZoneDetailed ResourceState where
  resourceState f x = fmap
    (\ y -> x { activityZoneDetailed_resourceState = y })
    (f (activityZoneDetailed_resourceState x))

instance ResourceStateLens AthleteDetailed ResourceState where
  resourceState f x = fmap
    (\ y -> x { athleteDetailed_resourceState = y })
    (f (athleteDetailed_resourceState x))

instance ResourceStateLens AthleteMeta ResourceState where
  resourceState f x = fmap
    (\ y -> x { athleteMeta_resourceState = y })
    (f (athleteMeta_resourceState x))

instance ResourceStateLens AthleteSummary ResourceState where
  resourceState f x = fmap
    (\ y -> x { athleteSummary_resourceState = y })
    (f (athleteSummary_resourceState x))

instance ResourceStateLens ClubDetailed ResourceState where
  resourceState f x = fmap
    (\ y -> x { clubDetailed_resourceState = y })
    (f (clubDetailed_resourceState x))

instance ResourceStateLens ClubSummary ResourceState where
  resourceState f x = fmap
    (\ y -> x { clubSummary_resourceState = y })
    (f (clubSummary_resourceState x))

instance ResourceStateLens CommentSummary ResourceState where
  resourceState f x = fmap
    (\ y -> x { commentSummary_resourceState = y })
    (f (commentSummary_resourceState x))

instance ResourceStateLens EffortDetailed ResourceState where
  resourceState f x = fmap
    (\ y -> x { effortDetailed_resourceState = y })
    (f (effortDetailed_resourceState x))

instance ResourceStateLens GearDetailed ResourceState where
  resourceState f x = fmap
    (\ y -> x { gearDetailed_resourceState = y })
    (f (gearDetailed_resourceState x))

instance ResourceStateLens GearSummary ResourceState where
  resourceState f x = fmap
    (\ y -> x { gearSummary_resourceState = y })
    (f (gearSummary_resourceState x))

instance ResourceStateLens PhotoSummary ResourceState where
  resourceState f x = fmap
    (\ y -> x { photoSummary_resourceState = y })
    (f (photoSummary_resourceState x))

instance ResourceStateLens PolylineDetailed ResourceState where
  resourceState f x = fmap
    (\ y -> x { polylineDetailed_resourceState = y })
    (f (polylineDetailed_resourceState x))

instance ResourceStateLens PolylineSummary ResourceState where
  resourceState f x = fmap
    (\ y -> x { polylineSummary_resourceState = y })
    (f (polylineSummary_resourceState x))

instance ResourceStateLens SegmentDetailed ResourceState where
  resourceState f x = fmap
    (\ y -> x { segmentDetailed_resourceState = y })
    (f (segmentDetailed_resourceState x))

instance ResourceStateLens SegmentExplorerEntry ResourceState where
  resourceState f x = fmap
    (\ y -> x { segmentExplorerEntry_resourceState = y })
    (f (segmentExplorerEntry_resourceState x))

instance ResourceStateLens SegmentSummary ResourceState where
  resourceState f x = fmap
    (\ y -> x { segmentSummary_resourceState = y })
    (f (segmentSummary_resourceState x))

instance SegmentEffortsLens ActivityDetailed [EffortDetailed] where
  segmentEfforts f x = fmap
    (\ y -> x { activityDetailed_segmentEfforts = y })
    (f (activityDetailed_segmentEfforts x))

instance SegmentLens EffortDetailed SegmentSummary where
  segment f x = fmap
    (\ y -> x { effortDetailed_segment = y })
    (f (effortDetailed_segment x))

instance SensorBasedLens ActivityZoneDetailed Bool where
  sensorBased f x = fmap
    (\ y -> x { activityZoneDetailed_sensorBased = y })
    (f (activityZoneDetailed_sensorBased x))

instance SeriesTypeLens GetStreamsOptions SeriesType where
  seriesType f x = fmap
    (\ y -> x { getStreamsOptions_seriesType = y })
    (f (getStreamsOptions_seriesType x))

instance SeriesTypeLens StreamDetailed SeriesType where
  seriesType f x = fmap
    (\ y -> x { streamDetailed_seriesType = y })
    (f (streamDetailed_seriesType x))

instance SexLens AthleteDetailed (Maybe Gender) where
  sex f x = fmap
    (\ y -> x { athleteDetailed_sex = y })
    (f (athleteDetailed_sex x))

instance SexLens AthleteSummary (Maybe Gender) where
  sex f x = fmap
    (\ y -> x { athleteSummary_sex = y })
    (f (athleteSummary_sex x))

instance SexLens UpdateCurrentAthleteOptions (Maybe Gender) where
  sex f x = fmap
    (\ y -> x { updateCurrentAthleteOptions_sex = y })
    (f (updateCurrentAthleteOptions_sex x))

instance ShoesLens AthleteDetailed [GearSummary] where
  shoes f x = fmap
    (\ y -> x { athleteDetailed_shoes = y })
    (f (athleteDetailed_shoes x))

instance SportTypeLens ClubDetailed SportType where
  sportType f x = fmap
    (\ y -> x { clubDetailed_sportType = y })
    (f (clubDetailed_sportType x))

instance StarCountLens SegmentDetailed Integer where
  starCount f x = fmap
    (\ y -> x { segmentDetailed_starCount = y })
    (f (segmentDetailed_starCount x))

instance StarredLens SegmentDetailed Bool where
  starred f x = fmap
    (\ y -> x { segmentDetailed_starred = y })
    (f (segmentDetailed_starred x))

instance StarredLens SegmentExplorerEntry Bool where
  starred f x = fmap
    (\ y -> x { segmentExplorerEntry_starred = y })
    (f (segmentExplorerEntry_starred x))

instance StarredLens SegmentSummary Bool where
  starred f x = fmap
    (\ y -> x { segmentSummary_starred = y })
    (f (segmentSummary_starred x))

instance StartDateLens ActivityDetailed UTCTime where
  startDate f x = fmap
    (\ y -> x { activityDetailed_startDate = y })
    (f (activityDetailed_startDate x))

instance StartDateLens ActivityLapSummary UTCTime where
  startDate f x = fmap
    (\ y -> x { activityLapSummary_startDate = y })
    (f (activityLapSummary_startDate x))

instance StartDateLens ActivitySummary UTCTime where
  startDate f x = fmap
    (\ y -> x { activitySummary_startDate = y })
    (f (activitySummary_startDate x))

instance StartDateLens EffortDetailed UTCTime where
  startDate f x = fmap
    (\ y -> x { effortDetailed_startDate = y })
    (f (effortDetailed_startDate x))

instance StartDateLens SegmentLeaderboardEntry UTCTime where
  startDate f x = fmap
    (\ y -> x { segmentLeaderboardEntry_startDate = y })
    (f (segmentLeaderboardEntry_startDate x))

instance StartDateLocalLens ActivityDetailed UTCTime where
  startDateLocal f x = fmap
    (\ y -> x { activityDetailed_startDateLocal = y })
    (f (activityDetailed_startDateLocal x))

instance StartDateLocalLens ActivityLapSummary UTCTime where
  startDateLocal f x = fmap
    (\ y -> x { activityLapSummary_startDateLocal = y })
    (f (activityLapSummary_startDateLocal x))

instance StartDateLocalLens ActivitySummary UTCTime where
  startDateLocal f x = fmap
    (\ y -> x { activitySummary_startDateLocal = y })
    (f (activitySummary_startDateLocal x))

instance StartDateLocalLens EffortDetailed UTCTime where
  startDateLocal f x = fmap
    (\ y -> x { effortDetailed_startDateLocal = y })
    (f (effortDetailed_startDateLocal x))

instance StartDateLocalLens SegmentLeaderboardEntry UTCTime where
  startDateLocal f x = fmap
    (\ y -> x { segmentLeaderboardEntry_startDateLocal = y })
    (f (segmentLeaderboardEntry_startDateLocal x))

instance StartIndexLens ActivityLapSummary Integer where
  startIndex f x = fmap
    (\ y -> x { activityLapSummary_startIndex = y })
    (f (activityLapSummary_startIndex x))

instance StartIndexLens EffortDetailed Integer where
  startIndex f x = fmap
    (\ y -> x { effortDetailed_startIndex = y })
    (f (effortDetailed_startIndex x))

instance StartLatitudeLens ActivityDetailed Double where
  startLatitude f x = fmap
    (\ y -> x { activityDetailed_startLatitude = y })
    (f (activityDetailed_startLatitude x))

instance StartLatitudeLens ActivitySummary Double where
  startLatitude f x = fmap
    (\ y -> x { activitySummary_startLatitude = y })
    (f (activitySummary_startLatitude x))

instance StartLatitudeLens SegmentDetailed Double where
  startLatitude f x = fmap
    (\ y -> x { segmentDetailed_startLatitude = y })
    (f (segmentDetailed_startLatitude x))

instance StartLatitudeLens SegmentSummary Double where
  startLatitude f x = fmap
    (\ y -> x { segmentSummary_startLatitude = y })
    (f (segmentSummary_startLatitude x))

instance StartLatlngLens ActivityDetailed (Maybe (Double, Double)) where
  startLatlng f x = fmap
    (\ y -> x { activityDetailed_startLatlng = y })
    (f (activityDetailed_startLatlng x))

instance StartLatlngLens ActivitySummary (Maybe (Double, Double)) where
  startLatlng f x = fmap
    (\ y -> x { activitySummary_startLatlng = y })
    (f (activitySummary_startLatlng x))

instance StartLatlngLens SegmentDetailed ((Double, Double)) where
  startLatlng f x = fmap
    (\ y -> x { segmentDetailed_startLatlng = y })
    (f (segmentDetailed_startLatlng x))

instance StartLatlngLens SegmentExplorerEntry ((Double, Double)) where
  startLatlng f x = fmap
    (\ y -> x { segmentExplorerEntry_startLatlng = y })
    (f (segmentExplorerEntry_startLatlng x))

instance StartLatlngLens SegmentSummary ((Double, Double)) where
  startLatlng f x = fmap
    (\ y -> x { segmentSummary_startLatlng = y })
    (f (segmentSummary_startLatlng x))

instance StartLongitudeLens ActivityDetailed Double where
  startLongitude f x = fmap
    (\ y -> x { activityDetailed_startLongitude = y })
    (f (activityDetailed_startLongitude x))

instance StartLongitudeLens ActivitySummary Double where
  startLongitude f x = fmap
    (\ y -> x { activitySummary_startLongitude = y })
    (f (activitySummary_startLongitude x))

instance StartLongitudeLens SegmentDetailed Double where
  startLongitude f x = fmap
    (\ y -> x { segmentDetailed_startLongitude = y })
    (f (segmentDetailed_startLongitude x))

instance StartLongitudeLens SegmentSummary Double where
  startLongitude f x = fmap
    (\ y -> x { segmentSummary_startLongitude = y })
    (f (segmentSummary_startLongitude x))

instance StateLens AthleteDetailed Text where
  state f x = fmap
    (\ y -> x { athleteDetailed_state = y })
    (f (athleteDetailed_state x))

instance StateLens AthleteSummary Text where
  state f x = fmap
    (\ y -> x { athleteSummary_state = y })
    (f (athleteSummary_state x))

instance StateLens BuildAuthorizeUrlOptions String where
  state f x = fmap
    (\ y -> x { buildAuthorizeUrlOptions_state = y })
    (f (buildAuthorizeUrlOptions_state x))

instance StateLens ClubDetailed Text where
  state f x = fmap
    (\ y -> x { clubDetailed_state = y })
    (f (clubDetailed_state x))

instance StateLens SegmentDetailed Text where
  state f x = fmap
    (\ y -> x { segmentDetailed_state = y })
    (f (segmentDetailed_state x))

instance StateLens SegmentSummary Text where
  state f x = fmap
    (\ y -> x { segmentSummary_state = y })
    (f (segmentSummary_state x))

instance StateLens UpdateCurrentAthleteOptions (Maybe String) where
  state f x = fmap
    (\ y -> x { updateCurrentAthleteOptions_state = y })
    (f (updateCurrentAthleteOptions_state x))

instance StatusLens UploadStatus Text where
  status f x = fmap
    (\ y -> x { uploadStatus_status = y })
    (f (uploadStatus_status x))

instance SummaryPolylineLens PolylineDetailed (Maybe [(Double, Double)]) where
  summaryPolyline f x = fmap
    (\ y -> x { polylineDetailed_summaryPolyline = y })
    (f (polylineDetailed_summaryPolyline x))

instance SummaryPolylineLens PolylineSummary (Maybe [(Double, Double)]) where
  summaryPolyline f x = fmap
    (\ y -> x { polylineSummary_summaryPolyline = y })
    (f (polylineSummary_summaryPolyline x))

instance TextLens CommentSummary Text where
  text f x = fmap
    (\ y -> x { commentSummary_text = y })
    (f (commentSummary_text x))

instance TimeLens ActivityZoneDistributionBucket Integer where
  time f x = fmap
    (\ y -> x { activityZoneDistributionBucket_time = y })
    (f (activityZoneDistributionBucket_time x))

instance TimezoneLens ActivityDetailed Text where
  timezone f x = fmap
    (\ y -> x { activityDetailed_timezone = y })
    (f (activityDetailed_timezone x))

instance TimezoneLens ActivitySummary Text where
  timezone f x = fmap
    (\ y -> x { activitySummary_timezone = y })
    (f (activitySummary_timezone x))

instance TotalElevationGainLens ActivityDetailed Double where
  totalElevationGain f x = fmap
    (\ y -> x { activityDetailed_totalElevationGain = y })
    (f (activityDetailed_totalElevationGain x))

instance TotalElevationGainLens ActivityLapSummary Double where
  totalElevationGain f x = fmap
    (\ y -> x { activityLapSummary_totalElevationGain = y })
    (f (activityLapSummary_totalElevationGain x))

instance TotalElevationGainLens ActivitySummary Double where
  totalElevationGain f x = fmap
    (\ y -> x { activitySummary_totalElevationGain = y })
    (f (activitySummary_totalElevationGain x))

instance TotalElevationGainLens SegmentDetailed Double where
  totalElevationGain f x = fmap
    (\ y -> x { segmentDetailed_totalElevationGain = y })
    (f (segmentDetailed_totalElevationGain x))

instance TrainerLens ActivityDetailed Bool where
  trainer f x = fmap
    (\ y -> x { activityDetailed_trainer = y })
    (f (activityDetailed_trainer x))

instance TrainerLens ActivitySummary Bool where
  trainer f x = fmap
    (\ y -> x { activitySummary_trainer = y })
    (f (activitySummary_trainer x))

instance TrainerLens UpdateActivityOptions (Maybe Bool) where
  trainer f x = fmap
    (\ y -> x { updateActivityOptions_trainer = y })
    (f (updateActivityOptions_trainer x))

instance TrainerLens UploadActivityOptions Bool where
  trainer f x = fmap
    (\ y -> x { uploadActivityOptions_trainer = y })
    (f (uploadActivityOptions_trainer x))

instance TruncatedLens ActivityDetailed Integer where
  truncated f x = fmap
    (\ y -> x { activityDetailed_truncated = y })
    (f (activityDetailed_truncated x))

instance TypeLens ActivityDetailed ActivityType where
  type_ f x = fmap
    (\ y -> x { activityDetailed_type = y })
    (f (activityDetailed_type x))

instance TypeLens ActivitySummary ActivityType where
  type_ f x = fmap
    (\ y -> x { activitySummary_type = y })
    (f (activitySummary_type x))

instance TypeLens ActivityZoneDetailed ActivityZoneType where
  type_ f x = fmap
    (\ y -> x { activityZoneDetailed_type = y })
    (f (activityZoneDetailed_type x))

instance TypeLens PhotoSummary PhotoType where
  type_ f x = fmap
    (\ y -> x { photoSummary_type = y })
    (f (photoSummary_type x))

instance TypeLens StreamDetailed Text where
  type_ f x = fmap
    (\ y -> x { streamDetailed_type = y })
    (f (streamDetailed_type x))

instance TypeLens UpdateActivityOptions (Maybe ActivityType) where
  type_ f x = fmap
    (\ y -> x { updateActivityOptions_type = y })
    (f (updateActivityOptions_type x))

instance UidLens PhotoSummary Text where
  uid f x = fmap
    (\ y -> x { photoSummary_uid = y })
    (f (photoSummary_uid x))

instance UpdatedAtLens AthleteDetailed UTCTime where
  updatedAt f x = fmap
    (\ y -> x { athleteDetailed_updatedAt = y })
    (f (athleteDetailed_updatedAt x))

instance UpdatedAtLens AthleteSummary UTCTime where
  updatedAt f x = fmap
    (\ y -> x { athleteSummary_updatedAt = y })
    (f (athleteSummary_updatedAt x))

instance UpdatedAtLens SegmentDetailed UTCTime where
  updatedAt f x = fmap
    (\ y -> x { segmentDetailed_updatedAt = y })
    (f (segmentDetailed_updatedAt x))

instance UploadIdLens ActivityDetailed (Maybe Integer) where
  uploadId f x = fmap
    (\ y -> x { activityDetailed_uploadId = y })
    (f (activityDetailed_uploadId x))

instance UploadIdLens ActivitySummary (Maybe Integer) where
  uploadId f x = fmap
    (\ y -> x { activitySummary_uploadId = y })
    (f (activitySummary_uploadId x))

instance UploadedAtLens PhotoSummary UTCTime where
  uploadedAt f x = fmap
    (\ y -> x { photoSummary_uploadedAt = y })
    (f (photoSummary_uploadedAt x))

instance WeightClassLens GetSegmentLeaderboardOptions (Maybe WeightClass) where
  weightClass f x = fmap
    (\ y -> x { getSegmentLeaderboardOptions_weightClass = y })
    (f (getSegmentLeaderboardOptions_weightClass x))

instance WeightLens UpdateCurrentAthleteOptions (Maybe Double) where
  weight f x = fmap
    (\ y -> x { updateCurrentAthleteOptions_weight = y })
    (f (updateCurrentAthleteOptions_weight x))

instance WriteScopeLens BuildAuthorizeUrlOptions Bool where
  writeScope f x = fmap
    (\ y -> x { buildAuthorizeUrlOptions_writeScope = y })
    (f (buildAuthorizeUrlOptions_writeScope x))
