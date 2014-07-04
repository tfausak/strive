{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}

-- | Lenses for easily getting and setting values.
module Strive.Lenses where

import Data.Aeson (Value)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Strive.Objects

-- | A lens for a record, returning a field and a residual.
type Lens a b = a -> (b, b -> a)

-- | Get a field from a record using a lens.
get :: Lens a b -> a -> b
get = (fst .)

-- | Set a field in a record using a lens.
set :: Lens a b -> b -> a -> a
set = flip . (snd .)

class AccessTokenLens a b | a -> b where
  accessToken' :: Lens a b
instance AccessTokenLens TokenExchangeResponse Text where
  accessToken' tokenExchangeResponse =
    ( tokenExchangeResponseAccessToken tokenExchangeResponse
    , \ tokenExchangeResponseAccessToken' -> tokenExchangeResponse { tokenExchangeResponseAccessToken = tokenExchangeResponseAccessToken' }
    )
instance AccessTokenLens DeauthorizationResponse Text where
  accessToken' deauthorizationResponse =
    ( deauthorizationResponseAccessToken deauthorizationResponse
    , \ deauthorizationResponseAccessToken' -> deauthorizationResponse { deauthorizationResponseAccessToken = deauthorizationResponseAccessToken' }
    )

class AchievementCountLens a b | a -> b where
  achievementCount :: Lens a b
instance AchievementCountLens ActivityDetailed Integer where
  achievementCount activityDetailed =
    ( activityDetailedAchievementCount activityDetailed
    , \ activityDetailedAchievementCount' -> activityDetailed { activityDetailedAchievementCount = activityDetailedAchievementCount' }
    )
instance AchievementCountLens ActivitySummary Integer where
  achievementCount activitySummary =
    ( activitySummaryAchievementCount activitySummary
    , \ activitySummaryAchievementCount' -> activitySummary { activitySummaryAchievementCount = activitySummaryAchievementCount' }
    )

class ActivityIdLens a b | a -> b where
  activityId :: Lens a b
instance ActivityIdLens ActivityLapSummary Integer where
  activityId activityLapSummary =
    ( activityLapSummaryActivityId activityLapSummary
    , \ activityLapSummaryActivityId' -> activityLapSummary { activityLapSummaryActivityId = activityLapSummaryActivityId' }
    )
instance ActivityIdLens CommentSummary Integer where
  activityId commentSummary =
    ( commentSummaryActivityId commentSummary
    , \ commentSummaryActivityId' -> commentSummary { commentSummaryActivityId = commentSummaryActivityId' }
    )
instance ActivityIdLens EffortDetailed Integer where
  activityId effortDetailed =
    ( effortDetailedActivityId effortDetailed
    , \ effortDetailedActivityId' -> effortDetailed { effortDetailedActivityId = effortDetailedActivityId' }
    )
instance ActivityIdLens PhotoSummary Integer where
  activityId photoSummary =
    ( photoSummaryActivityId photoSummary
    , \ photoSummaryActivityId' -> photoSummary { photoSummaryActivityId = photoSummaryActivityId' }
    )
instance ActivityIdLens SegmentLeaderboardEntry Integer where
  activityId segmentLeaderboardEntry =
    ( segmentLeaderboardEntryActivityId segmentLeaderboardEntry
    , \ segmentLeaderboardEntryActivityId' -> segmentLeaderboardEntry { segmentLeaderboardEntryActivityId = segmentLeaderboardEntryActivityId' }
    )
instance ActivityIdLens UploadStatus (Maybe Integer) where
  activityId uploadStatus =
    ( uploadStatusActivityId uploadStatus
    , \ uploadStatusActivityId' -> uploadStatus { uploadStatusActivityId = uploadStatusActivityId' }
    )

class ActivityTypeLens a b | a -> b where
  activityType :: Lens a b
instance ActivityTypeLens SegmentDetailed Text where
  activityType segmentDetailed =
    ( segmentDetailedActivityType segmentDetailed
    , \ segmentDetailedActivityType' -> segmentDetailed { segmentDetailedActivityType = segmentDetailedActivityType' }
    )
instance ActivityTypeLens SegmentSummary Text where
  activityType segmentSummary =
    ( segmentSummaryActivityType segmentSummary
    , \ segmentSummaryActivityType' -> segmentSummary { segmentSummaryActivityType = segmentSummaryActivityType' }
    )

class AthleteCountLens a b | a -> b where
  athleteCount :: Lens a b
instance AthleteCountLens ActivityDetailed Integer where
  athleteCount activityDetailed =
    ( activityDetailedAthleteCount activityDetailed
    , \ activityDetailedAthleteCount' -> activityDetailed { activityDetailedAthleteCount = activityDetailedAthleteCount' }
    )
instance AthleteCountLens ActivitySummary Integer where
  athleteCount activitySummary =
    ( activitySummaryAthleteCount activitySummary
    , \ activitySummaryAthleteCount' -> activitySummary { activitySummaryAthleteCount = activitySummaryAthleteCount' }
    )
instance AthleteCountLens SegmentDetailed Integer where
  athleteCount segmentDetailed =
    ( segmentDetailedAthleteCount segmentDetailed
    , \ segmentDetailedAthleteCount' -> segmentDetailed { segmentDetailedAthleteCount = segmentDetailedAthleteCount' }
    )

class AthleteGenderLens a b | a -> b where
  athleteGender :: Lens a b
instance AthleteGenderLens SegmentLeaderboardEntry (Maybe Char) where
  athleteGender segmentLeaderboardEntry =
    ( segmentLeaderboardEntryAthleteGender segmentLeaderboardEntry
    , \ segmentLeaderboardEntryAthleteGender' -> segmentLeaderboardEntry { segmentLeaderboardEntryAthleteGender = segmentLeaderboardEntryAthleteGender' }
    )

class AthleteIdLens a b | a -> b where
  athleteId :: Lens a b
instance AthleteIdLens ActivityLapSummary Integer where
  athleteId activityLapSummary =
    ( activityLapSummaryAthleteId activityLapSummary
    , \ activityLapSummaryAthleteId' -> activityLapSummary { activityLapSummaryAthleteId = activityLapSummaryAthleteId' }
    )
instance AthleteIdLens EffortDetailed Integer where
  athleteId effortDetailed =
    ( effortDetailedAthleteId effortDetailed
    , \ effortDetailedAthleteId' -> effortDetailed { effortDetailedAthleteId = effortDetailedAthleteId' }
    )
instance AthleteIdLens SegmentLeaderboardEntry Integer where
  athleteId segmentLeaderboardEntry =
    ( segmentLeaderboardEntryAthleteId segmentLeaderboardEntry
    , \ segmentLeaderboardEntryAthleteId' -> segmentLeaderboardEntry { segmentLeaderboardEntryAthleteId = segmentLeaderboardEntryAthleteId' }
    )

class AthleteLens a b | a -> b where
  athlete :: Lens a b
instance AthleteLens ActivityDetailed AthleteMeta where
  athlete activityDetailed =
    ( activityDetailedAthlete activityDetailed
    , \ activityDetailedAthlete' -> activityDetailed { activityDetailedAthlete = activityDetailedAthlete' }
    )
instance AthleteLens ActivitySummary AthleteMeta where
  athlete activitySummary =
    ( activitySummaryAthlete activitySummary
    , \ activitySummaryAthlete' -> activitySummary { activitySummaryAthlete = activitySummaryAthlete' }
    )
instance AthleteLens TokenExchangeResponse AthleteDetailed where
  athlete tokenExchangeResponse =
    ( tokenExchangeResponseAthlete tokenExchangeResponse
    , \ tokenExchangeResponseAthlete' -> tokenExchangeResponse { tokenExchangeResponseAthlete = tokenExchangeResponseAthlete' }
    )
instance AthleteLens CommentSummary AthleteSummary where
  athlete commentSummary =
    ( commentSummaryAthlete commentSummary
    , \ commentSummaryAthlete' -> commentSummary { commentSummaryAthlete = commentSummaryAthlete' }
    )

class AthleteNameLens a b | a -> b where
  athleteName :: Lens a b
instance AthleteNameLens SegmentLeaderboardEntry Text where
  athleteName segmentLeaderboardEntry =
    ( segmentLeaderboardEntryAthleteName segmentLeaderboardEntry
    , \ segmentLeaderboardEntryAthleteName' -> segmentLeaderboardEntry { segmentLeaderboardEntryAthleteName = segmentLeaderboardEntryAthleteName' }
    )

class AthleteProfileLens a b | a -> b where
  athleteProfile :: Lens a b
instance AthleteProfileLens SegmentLeaderboardEntry Text where
  athleteProfile segmentLeaderboardEntry =
    ( segmentLeaderboardEntryAthleteProfile segmentLeaderboardEntry
    , \ segmentLeaderboardEntryAthleteProfile' -> segmentLeaderboardEntry { segmentLeaderboardEntryAthleteProfile = segmentLeaderboardEntryAthleteProfile' }
    )

class AverageCadenceLens a b | a -> b where
  averageCadence :: Lens a b
instance AverageCadenceLens EffortDetailed (Maybe Double) where
  averageCadence effortDetailed =
    ( effortDetailedAverageCadence effortDetailed
    , \ effortDetailedAverageCadence' -> effortDetailed { effortDetailedAverageCadence = effortDetailedAverageCadence' }
    )

class AverageGradeLens a b | a -> b where
  averageGrade :: Lens a b
instance AverageGradeLens SegmentDetailed Double where
  averageGrade segmentDetailed =
    ( segmentDetailedAverageGrade segmentDetailed
    , \ segmentDetailedAverageGrade' -> segmentDetailed { segmentDetailedAverageGrade = segmentDetailedAverageGrade' }
    )
instance AverageGradeLens SegmentSummary Double where
  averageGrade segmentSummary =
    ( segmentSummaryAverageGrade segmentSummary
    , \ segmentSummaryAverageGrade' -> segmentSummary { segmentSummaryAverageGrade = segmentSummaryAverageGrade' }
    )

class AverageHeartrateLens a b | a -> b where
  averageHeartrate :: Lens a b
instance AverageHeartrateLens EffortDetailed (Maybe Double) where
  averageHeartrate effortDetailed =
    ( effortDetailedAverageHeartrate effortDetailed
    , \ effortDetailedAverageHeartrate' -> effortDetailed { effortDetailedAverageHeartrate = effortDetailedAverageHeartrate' }
    )

class AverageHrLens a b | a -> b where
  averageHr :: Lens a b
instance AverageHrLens SegmentLeaderboardEntry Double where
  averageHr segmentLeaderboardEntry =
    ( segmentLeaderboardEntryAverageHr segmentLeaderboardEntry
    , \ segmentLeaderboardEntryAverageHr' -> segmentLeaderboardEntry { segmentLeaderboardEntryAverageHr = segmentLeaderboardEntryAverageHr' }
    )

class AverageSpeedLens a b | a -> b where
  averageSpeed :: Lens a b
instance AverageSpeedLens ActivityDetailed Double where
  averageSpeed activityDetailed =
    ( activityDetailedAverageSpeed activityDetailed
    , \ activityDetailedAverageSpeed' -> activityDetailed { activityDetailedAverageSpeed = activityDetailedAverageSpeed' }
    )
instance AverageSpeedLens ActivitySummary Double where
  averageSpeed activitySummary =
    ( activitySummaryAverageSpeed activitySummary
    , \ activitySummaryAverageSpeed' -> activitySummary { activitySummaryAverageSpeed = activitySummaryAverageSpeed' }
    )
instance AverageSpeedLens ActivityLapSummary Double where
  averageSpeed activityLapSummary =
    ( activityLapSummaryAverageSpeed activityLapSummary
    , \ activityLapSummaryAverageSpeed' -> activityLapSummary { activityLapSummaryAverageSpeed = activityLapSummaryAverageSpeed' }
    )

class AverageWattsLens a b | a -> b where
  averageWatts :: Lens a b
instance AverageWattsLens ActivityDetailed (Maybe Double) where
  averageWatts activityDetailed =
    ( activityDetailedAverageWatts activityDetailed
    , \ activityDetailedAverageWatts' -> activityDetailed { activityDetailedAverageWatts = activityDetailedAverageWatts' }
    )
instance AverageWattsLens ActivitySummary (Maybe Double) where
  averageWatts activitySummary =
    ( activitySummaryAverageWatts activitySummary
    , \ activitySummaryAverageWatts' -> activitySummary { activitySummaryAverageWatts = activitySummaryAverageWatts' }
    )
instance AverageWattsLens ActivityLapSummary Double where
  averageWatts activityLapSummary =
    ( activityLapSummaryAverageWatts activityLapSummary
    , \ activityLapSummaryAverageWatts' -> activityLapSummary { activityLapSummaryAverageWatts = activityLapSummaryAverageWatts' }
    )
instance AverageWattsLens EffortDetailed (Maybe Double) where
  averageWatts effortDetailed =
    ( effortDetailedAverageWatts effortDetailed
    , \ effortDetailedAverageWatts' -> effortDetailed { effortDetailedAverageWatts = effortDetailedAverageWatts' }
    )
instance AverageWattsLens SegmentLeaderboardEntry Double where
  averageWatts segmentLeaderboardEntry =
    ( segmentLeaderboardEntryAverageWatts segmentLeaderboardEntry
    , \ segmentLeaderboardEntryAverageWatts' -> segmentLeaderboardEntry { segmentLeaderboardEntryAverageWatts = segmentLeaderboardEntryAverageWatts' }
    )

class AvgGradeLens a b | a -> b where
  avgGrade :: Lens a b
instance AvgGradeLens SegmentExplorerEntry Double where
  avgGrade segmentExplorerEntry =
    ( segmentExplorerEntryAvgGrade segmentExplorerEntry
    , \ segmentExplorerEntryAvgGrade' -> segmentExplorerEntry { segmentExplorerEntryAvgGrade = segmentExplorerEntryAvgGrade' }
    )

class BikesLens a b | a -> b where
  bikes :: Lens a b
instance BikesLens AthleteDetailed [GearSummary] where
  bikes athleteDetailed =
    ( athleteDetailedBikes athleteDetailed
    , \ athleteDetailedBikes' -> athleteDetailed { athleteDetailedBikes = athleteDetailedBikes' }
    )

class BrandNameLens a b | a -> b where
  brandName :: Lens a b
instance BrandNameLens GearDetailed Text where
  brandName gearDetailed =
    ( gearDetailedBrandName gearDetailed
    , \ gearDetailedBrandName' -> gearDetailed { gearDetailedBrandName = gearDetailedBrandName' }
    )

class CaloriesLens a b | a -> b where
  calories :: Lens a b
instance CaloriesLens ActivityDetailed Double where
  calories activityDetailed =
    ( activityDetailedCalories activityDetailed
    , \ activityDetailedCalories' -> activityDetailed { activityDetailedCalories = activityDetailedCalories' }
    )

class CaptionLens a b | a -> b where
  caption :: Lens a b
instance CaptionLens PhotoSummary Text where
  caption photoSummary =
    ( photoSummaryCaption photoSummary
    , \ photoSummaryCaption' -> photoSummary { photoSummaryCaption = photoSummaryCaption' }
    )

class CityLens a b | a -> b where
  city :: Lens a b
instance CityLens AthleteDetailed Text where
  city athleteDetailed =
    ( athleteDetailedCity athleteDetailed
    , \ athleteDetailedCity' -> athleteDetailed { athleteDetailedCity = athleteDetailedCity' }
    )
instance CityLens AthleteSummary (Maybe Text) where
  city athleteSummary =
    ( athleteSummaryCity athleteSummary
    , \ athleteSummaryCity' -> athleteSummary { athleteSummaryCity = athleteSummaryCity' }
    )
instance CityLens ClubDetailed Text where
  city clubDetailed =
    ( clubDetailedCity clubDetailed
    , \ clubDetailedCity' -> clubDetailed { clubDetailedCity = clubDetailedCity' }
    )
instance CityLens SegmentDetailed Text where
  city segmentDetailed =
    ( segmentDetailedCity segmentDetailed
    , \ segmentDetailedCity' -> segmentDetailed { segmentDetailedCity = segmentDetailedCity' }
    )
instance CityLens SegmentSummary Text where
  city segmentSummary =
    ( segmentSummaryCity segmentSummary
    , \ segmentSummaryCity' -> segmentSummary { segmentSummaryCity = segmentSummaryCity' }
    )

class ClimbCategoryDescLens a b | a -> b where
  climbCategoryDesc :: Lens a b
instance ClimbCategoryDescLens SegmentExplorerEntry String where
  climbCategoryDesc segmentExplorerEntry =
    ( segmentExplorerEntryClimbCategoryDesc segmentExplorerEntry
    , \ segmentExplorerEntryClimbCategoryDesc' -> segmentExplorerEntry { segmentExplorerEntryClimbCategoryDesc = segmentExplorerEntryClimbCategoryDesc' }
    )

class ClimbCategoryLens a b | a -> b where
  climbCategory :: Lens a b
instance ClimbCategoryLens SegmentDetailed Integer where
  climbCategory segmentDetailed =
    ( segmentDetailedClimbCategory segmentDetailed
    , \ segmentDetailedClimbCategory' -> segmentDetailed { segmentDetailedClimbCategory = segmentDetailedClimbCategory' }
    )
instance ClimbCategoryLens SegmentSummary Integer where
  climbCategory segmentSummary =
    ( segmentSummaryClimbCategory segmentSummary
    , \ segmentSummaryClimbCategory' -> segmentSummary { segmentSummaryClimbCategory = segmentSummaryClimbCategory' }
    )
instance ClimbCategoryLens SegmentExplorerEntry Integer where
  climbCategory segmentExplorerEntry =
    ( segmentExplorerEntryClimbCategory segmentExplorerEntry
    , \ segmentExplorerEntryClimbCategory' -> segmentExplorerEntry { segmentExplorerEntryClimbCategory = segmentExplorerEntryClimbCategory' }
    )

class ClubTypeLens a b | a -> b where
  clubType :: Lens a b
instance ClubTypeLens ClubDetailed Text where
  clubType clubDetailed =
    ( clubDetailedClubType clubDetailed
    , \ clubDetailedClubType' -> clubDetailed { clubDetailedClubType = clubDetailedClubType' }
    )

class ClubsLens a b | a -> b where
  clubs :: Lens a b
instance ClubsLens AthleteDetailed [ClubSummary] where
  clubs athleteDetailed =
    ( athleteDetailedClubs athleteDetailed
    , \ athleteDetailedClubs' -> athleteDetailed { athleteDetailedClubs = athleteDetailedClubs' }
    )

class CommentCountLens a b | a -> b where
  commentCount :: Lens a b
instance CommentCountLens ActivityDetailed Integer where
  commentCount activityDetailed =
    ( activityDetailedCommentCount activityDetailed
    , \ activityDetailedCommentCount' -> activityDetailed { activityDetailedCommentCount = activityDetailedCommentCount' }
    )
instance CommentCountLens ActivitySummary Integer where
  commentCount activitySummary =
    ( activitySummaryCommentCount activitySummary
    , \ activitySummaryCommentCount' -> activitySummary { activitySummaryCommentCount = activitySummaryCommentCount' }
    )

class CommuteLens a b | a -> b where
  commute :: Lens a b
instance CommuteLens ActivityDetailed Bool where
  commute activityDetailed =
    ( activityDetailedCommute activityDetailed
    , \ activityDetailedCommute' -> activityDetailed { activityDetailedCommute = activityDetailedCommute' }
    )
instance CommuteLens ActivitySummary Bool where
  commute activitySummary =
    ( activitySummaryCommute activitySummary
    , \ activitySummaryCommute' -> activitySummary { activitySummaryCommute = activitySummaryCommute' }
    )

class CountryLens a b | a -> b where
  country :: Lens a b
instance CountryLens AthleteDetailed Text where
  country athleteDetailed =
    ( athleteDetailedCountry athleteDetailed
    , \ athleteDetailedCountry' -> athleteDetailed { athleteDetailedCountry = athleteDetailedCountry' }
    )
instance CountryLens AthleteSummary (Maybe Text) where
  country athleteSummary =
    ( athleteSummaryCountry athleteSummary
    , \ athleteSummaryCountry' -> athleteSummary { athleteSummaryCountry = athleteSummaryCountry' }
    )
instance CountryLens ClubDetailed Text where
  country clubDetailed =
    ( clubDetailedCountry clubDetailed
    , \ clubDetailedCountry' -> clubDetailed { clubDetailedCountry = clubDetailedCountry' }
    )
instance CountryLens SegmentDetailed Text where
  country segmentDetailed =
    ( segmentDetailedCountry segmentDetailed
    , \ segmentDetailedCountry' -> segmentDetailed { segmentDetailedCountry = segmentDetailedCountry' }
    )
instance CountryLens SegmentSummary Text where
  country segmentSummary =
    ( segmentSummaryCountry segmentSummary
    , \ segmentSummaryCountry' -> segmentSummary { segmentSummaryCountry = segmentSummaryCountry' }
    )

class CreatedAtLens a b | a -> b where
  createdAt :: Lens a b
instance CreatedAtLens AthleteDetailed UTCTime where
  createdAt athleteDetailed =
    ( athleteDetailedCreatedAt athleteDetailed
    , \ athleteDetailedCreatedAt' -> athleteDetailed { athleteDetailedCreatedAt = athleteDetailedCreatedAt' }
    )
instance CreatedAtLens AthleteSummary UTCTime where
  createdAt athleteSummary =
    ( athleteSummaryCreatedAt athleteSummary
    , \ athleteSummaryCreatedAt' -> athleteSummary { athleteSummaryCreatedAt = athleteSummaryCreatedAt' }
    )
instance CreatedAtLens CommentSummary UTCTime where
  createdAt commentSummary =
    ( commentSummaryCreatedAt commentSummary
    , \ commentSummaryCreatedAt' -> commentSummary { commentSummaryCreatedAt = commentSummaryCreatedAt' }
    )
instance CreatedAtLens PhotoSummary UTCTime where
  createdAt photoSummary =
    ( photoSummaryCreatedAt photoSummary
    , \ photoSummaryCreatedAt' -> photoSummary { photoSummaryCreatedAt = photoSummaryCreatedAt' }
    )
instance CreatedAtLens SegmentDetailed UTCTime where
  createdAt segmentDetailed =
    ( segmentDetailedCreatedAt segmentDetailed
    , \ segmentDetailedCreatedAt' -> segmentDetailed { segmentDetailedCreatedAt = segmentDetailedCreatedAt' }
    )

class DataLens a b | a -> b where
  data' :: Lens a b
instance DataLens StreamDetailed [Value] where
  data' streamDetailed =
    ( streamDetailedData streamDetailed
    , \ streamDetailedData' -> streamDetailed { streamDetailedData = streamDetailedData' }
    )

class DatePreferenceLens a b | a -> b where
  datePreference :: Lens a b
instance DatePreferenceLens AthleteDetailed Text where
  datePreference athleteDetailed =
    ( athleteDetailedDatePreference athleteDetailed
    , \ athleteDetailedDatePreference' -> athleteDetailed { athleteDetailedDatePreference = athleteDetailedDatePreference' }
    )

class DescriptionLens a b | a -> b where
  description :: Lens a b
instance DescriptionLens ActivityDetailed Text where
  description activityDetailed =
    ( activityDetailedDescription activityDetailed
    , \ activityDetailedDescription' -> activityDetailed { activityDetailedDescription = activityDetailedDescription' }
    )
instance DescriptionLens ClubDetailed Text where
  description clubDetailed =
    ( clubDetailedDescription clubDetailed
    , \ clubDetailedDescription' -> clubDetailed { clubDetailedDescription = clubDetailedDescription' }
    )
instance DescriptionLens GearDetailed Text where
  description gearDetailed =
    ( gearDetailedDescription gearDetailed
    , \ gearDetailedDescription' -> gearDetailed { gearDetailedDescription = gearDetailedDescription' }
    )

class DistanceLens a b | a -> b where
  distance :: Lens a b
instance DistanceLens ActivityDetailed Double where
  distance activityDetailed =
    ( activityDetailedDistance activityDetailed
    , \ activityDetailedDistance' -> activityDetailed { activityDetailedDistance = activityDetailedDistance' }
    )
instance DistanceLens ActivitySummary Double where
  distance activitySummary =
    ( activitySummaryDistance activitySummary
    , \ activitySummaryDistance' -> activitySummary { activitySummaryDistance = activitySummaryDistance' }
    )
instance DistanceLens ActivityLapSummary Double where
  distance activityLapSummary =
    ( activityLapSummaryDistance activityLapSummary
    , \ activityLapSummaryDistance' -> activityLapSummary { activityLapSummaryDistance = activityLapSummaryDistance' }
    )
instance DistanceLens EffortDetailed Double where
  distance effortDetailed =
    ( effortDetailedDistance effortDetailed
    , \ effortDetailedDistance' -> effortDetailed { effortDetailedDistance = effortDetailedDistance' }
    )
instance DistanceLens GearDetailed Double where
  distance gearDetailed =
    ( gearDetailedDistance gearDetailed
    , \ gearDetailedDistance' -> gearDetailed { gearDetailedDistance = gearDetailedDistance' }
    )
instance DistanceLens GearSummary Double where
  distance gearSummary =
    ( gearSummaryDistance gearSummary
    , \ gearSummaryDistance' -> gearSummary { gearSummaryDistance = gearSummaryDistance' }
    )
instance DistanceLens SegmentDetailed Double where
  distance segmentDetailed =
    ( segmentDetailedDistance segmentDetailed
    , \ segmentDetailedDistance' -> segmentDetailed { segmentDetailedDistance = segmentDetailedDistance' }
    )
instance DistanceLens SegmentSummary Double where
  distance segmentSummary =
    ( segmentSummaryDistance segmentSummary
    , \ segmentSummaryDistance' -> segmentSummary { segmentSummaryDistance = segmentSummaryDistance' }
    )
instance DistanceLens SegmentLeaderboardEntry Double where
  distance segmentLeaderboardEntry =
    ( segmentLeaderboardEntryDistance segmentLeaderboardEntry
    , \ segmentLeaderboardEntryDistance' -> segmentLeaderboardEntry { segmentLeaderboardEntryDistance = segmentLeaderboardEntryDistance' }
    )
instance DistanceLens SegmentExplorerEntry Double where
  distance segmentExplorerEntry =
    ( segmentExplorerEntryDistance segmentExplorerEntry
    , \ segmentExplorerEntryDistance' -> segmentExplorerEntry { segmentExplorerEntryDistance = segmentExplorerEntryDistance' }
    )

class DistributionBucketsLens a b | a -> b where
  distributionBuckets :: Lens a b
instance DistributionBucketsLens ActivityZoneDetailed [ActivityZoneDistributionBucket] where
  distributionBuckets activityZoneDetailed =
    ( activityZoneDetailedDistributionBuckets activityZoneDetailed
    , \ activityZoneDetailedDistributionBuckets' -> activityZoneDetailed { activityZoneDetailedDistributionBuckets = activityZoneDetailedDistributionBuckets' }
    )

class EffortCountLens a b | a -> b where
  effortCount :: Lens a b
instance EffortCountLens SegmentDetailed Integer where
  effortCount segmentDetailed =
    ( segmentDetailedEffortCount segmentDetailed
    , \ segmentDetailedEffortCount' -> segmentDetailed { segmentDetailedEffortCount = segmentDetailedEffortCount' }
    )

class EffortIdLens a b | a -> b where
  effortId :: Lens a b
instance EffortIdLens SegmentLeaderboardEntry Integer where
  effortId segmentLeaderboardEntry =
    ( segmentLeaderboardEntryEffortId segmentLeaderboardEntry
    , \ segmentLeaderboardEntryEffortId' -> segmentLeaderboardEntry { segmentLeaderboardEntryEffortId = segmentLeaderboardEntryEffortId' }
    )

class ElapsedTimeLens a b | a -> b where
  elapsedTime :: Lens a b
instance ElapsedTimeLens ActivityDetailed Integer where
  elapsedTime activityDetailed =
    ( activityDetailedElapsedTime activityDetailed
    , \ activityDetailedElapsedTime' -> activityDetailed { activityDetailedElapsedTime = activityDetailedElapsedTime' }
    )
instance ElapsedTimeLens ActivitySummary Integer where
  elapsedTime activitySummary =
    ( activitySummaryElapsedTime activitySummary
    , \ activitySummaryElapsedTime' -> activitySummary { activitySummaryElapsedTime = activitySummaryElapsedTime' }
    )
instance ElapsedTimeLens ActivityLapSummary Integer where
  elapsedTime activityLapSummary =
    ( activityLapSummaryElapsedTime activityLapSummary
    , \ activityLapSummaryElapsedTime' -> activityLapSummary { activityLapSummaryElapsedTime = activityLapSummaryElapsedTime' }
    )
instance ElapsedTimeLens EffortDetailed Integer where
  elapsedTime effortDetailed =
    ( effortDetailedElapsedTime effortDetailed
    , \ effortDetailedElapsedTime' -> effortDetailed { effortDetailedElapsedTime = effortDetailedElapsedTime' }
    )
instance ElapsedTimeLens SegmentLeaderboardEntry Integer where
  elapsedTime segmentLeaderboardEntry =
    ( segmentLeaderboardEntryElapsedTime segmentLeaderboardEntry
    , \ segmentLeaderboardEntryElapsedTime' -> segmentLeaderboardEntry { segmentLeaderboardEntryElapsedTime = segmentLeaderboardEntryElapsedTime' }
    )

class ElevDifferenceLens a b | a -> b where
  elevDifference :: Lens a b
instance ElevDifferenceLens SegmentExplorerEntry Double where
  elevDifference segmentExplorerEntry =
    ( segmentExplorerEntryElevDifference segmentExplorerEntry
    , \ segmentExplorerEntryElevDifference' -> segmentExplorerEntry { segmentExplorerEntryElevDifference = segmentExplorerEntryElevDifference' }
    )

class ElevationHighLens a b | a -> b where
  elevationHigh :: Lens a b
instance ElevationHighLens SegmentDetailed Double where
  elevationHigh segmentDetailed =
    ( segmentDetailedElevationHigh segmentDetailed
    , \ segmentDetailedElevationHigh' -> segmentDetailed { segmentDetailedElevationHigh = segmentDetailedElevationHigh' }
    )
instance ElevationHighLens SegmentSummary Double where
  elevationHigh segmentSummary =
    ( segmentSummaryElevationHigh segmentSummary
    , \ segmentSummaryElevationHigh' -> segmentSummary { segmentSummaryElevationHigh = segmentSummaryElevationHigh' }
    )

class ElevationLowLens a b | a -> b where
  elevationLow :: Lens a b
instance ElevationLowLens SegmentDetailed Double where
  elevationLow segmentDetailed =
    ( segmentDetailedElevationLow segmentDetailed
    , \ segmentDetailedElevationLow' -> segmentDetailed { segmentDetailedElevationLow = segmentDetailedElevationLow' }
    )
instance ElevationLowLens SegmentSummary Double where
  elevationLow segmentSummary =
    ( segmentSummaryElevationLow segmentSummary
    , \ segmentSummaryElevationLow' -> segmentSummary { segmentSummaryElevationLow = segmentSummaryElevationLow' }
    )

class EmailLens a b | a -> b where
  email :: Lens a b
instance EmailLens AthleteDetailed Text where
  email athleteDetailed =
    ( athleteDetailedEmail athleteDetailed
    , \ athleteDetailedEmail' -> athleteDetailed { athleteDetailedEmail = athleteDetailedEmail' }
    )

class EndIndexLens a b | a -> b where
  endIndex :: Lens a b
instance EndIndexLens ActivityLapSummary Integer where
  endIndex activityLapSummary =
    ( activityLapSummaryEndIndex activityLapSummary
    , \ activityLapSummaryEndIndex' -> activityLapSummary { activityLapSummaryEndIndex = activityLapSummaryEndIndex' }
    )
instance EndIndexLens EffortDetailed Integer where
  endIndex effortDetailed =
    ( effortDetailedEndIndex effortDetailed
    , \ effortDetailedEndIndex' -> effortDetailed { effortDetailedEndIndex = effortDetailedEndIndex' }
    )

class EndLatitudeLens a b | a -> b where
  endLatitude :: Lens a b
instance EndLatitudeLens SegmentDetailed Double where
  endLatitude segmentDetailed =
    ( segmentDetailedEndLatitude segmentDetailed
    , \ segmentDetailedEndLatitude' -> segmentDetailed { segmentDetailedEndLatitude = segmentDetailedEndLatitude' }
    )
instance EndLatitudeLens SegmentSummary Double where
  endLatitude segmentSummary =
    ( segmentSummaryEndLatitude segmentSummary
    , \ segmentSummaryEndLatitude' -> segmentSummary { segmentSummaryEndLatitude = segmentSummaryEndLatitude' }
    )

class EndLatlngLens a b | a -> b where
  endLatlng :: Lens a b
instance EndLatlngLens ActivityDetailed (Maybe (Double, Double)) where
  endLatlng activityDetailed =
    ( activityDetailedEndLatlng activityDetailed
    , \ activityDetailedEndLatlng' -> activityDetailed { activityDetailedEndLatlng = activityDetailedEndLatlng' }
    )
instance EndLatlngLens ActivitySummary (Maybe (Double, Double)) where
  endLatlng activitySummary =
    ( activitySummaryEndLatlng activitySummary
    , \ activitySummaryEndLatlng' -> activitySummary { activitySummaryEndLatlng = activitySummaryEndLatlng' }
    )
instance EndLatlngLens SegmentDetailed ((Double, Double)) where
  endLatlng segmentDetailed =
    ( segmentDetailedEndLatlng segmentDetailed
    , \ segmentDetailedEndLatlng' -> segmentDetailed { segmentDetailedEndLatlng = segmentDetailedEndLatlng' }
    )
instance EndLatlngLens SegmentSummary ((Double, Double)) where
  endLatlng segmentSummary =
    ( segmentSummaryEndLatlng segmentSummary
    , \ segmentSummaryEndLatlng' -> segmentSummary { segmentSummaryEndLatlng = segmentSummaryEndLatlng' }
    )
instance EndLatlngLens SegmentExplorerEntry ((Double, Double)) where
  endLatlng segmentExplorerEntry =
    ( segmentExplorerEntryEndLatlng segmentExplorerEntry
    , \ segmentExplorerEntryEndLatlng' -> segmentExplorerEntry { segmentExplorerEntryEndLatlng = segmentExplorerEntryEndLatlng' }
    )

class EndLongitudeLens a b | a -> b where
  endLongitude :: Lens a b
instance EndLongitudeLens SegmentDetailed Double where
  endLongitude segmentDetailed =
    ( segmentDetailedEndLongitude segmentDetailed
    , \ segmentDetailedEndLongitude' -> segmentDetailed { segmentDetailedEndLongitude = segmentDetailedEndLongitude' }
    )
instance EndLongitudeLens SegmentSummary Double where
  endLongitude segmentSummary =
    ( segmentSummaryEndLongitude segmentSummary
    , \ segmentSummaryEndLongitude' -> segmentSummary { segmentSummaryEndLongitude = segmentSummaryEndLongitude' }
    )

class EntriesLens a b | a -> b where
  entries :: Lens a b
instance EntriesLens SegmentLeaderboard [SegmentLeaderboardEntry] where
  entries segmentLeaderboard =
    ( segmentLeaderboardEntries segmentLeaderboard
    , \ segmentLeaderboardEntries' -> segmentLeaderboard { segmentLeaderboardEntries = segmentLeaderboardEntries' }
    )
instance EntriesLens SegmentExplorer [SegmentExplorerEntry] where
  entries segmentExplorer =
    ( segmentExplorerEntries segmentExplorer
    , \ segmentExplorerEntries' -> segmentExplorer { segmentExplorerEntries = segmentExplorerEntries' }
    )

class ErrorLens a b | a -> b where
  error :: Lens a b
instance ErrorLens UploadStatus (Maybe Text) where
  error uploadStatus =
    ( uploadStatusError uploadStatus
    , \ uploadStatusError' -> uploadStatus { uploadStatusError = uploadStatusError' }
    )

class ExternalIdLens a b | a -> b where
  externalId :: Lens a b
instance ExternalIdLens ActivityDetailed (Maybe Text) where
  externalId activityDetailed =
    ( activityDetailedExternalId activityDetailed
    , \ activityDetailedExternalId' -> activityDetailed { activityDetailedExternalId = activityDetailedExternalId' }
    )
instance ExternalIdLens ActivitySummary (Maybe Text) where
  externalId activitySummary =
    ( activitySummaryExternalId activitySummary
    , \ activitySummaryExternalId' -> activitySummary { activitySummaryExternalId = activitySummaryExternalId' }
    )
instance ExternalIdLens UploadStatus Text where
  externalId uploadStatus =
    ( uploadStatusExternalId uploadStatus
    , \ uploadStatusExternalId' -> uploadStatus { uploadStatusExternalId = uploadStatusExternalId' }
    )

class FirstnameLens a b | a -> b where
  firstname :: Lens a b
instance FirstnameLens AthleteDetailed Text where
  firstname athleteDetailed =
    ( athleteDetailedFirstname athleteDetailed
    , \ athleteDetailedFirstname' -> athleteDetailed { athleteDetailedFirstname = athleteDetailedFirstname' }
    )
instance FirstnameLens AthleteSummary Text where
  firstname athleteSummary =
    ( athleteSummaryFirstname athleteSummary
    , \ athleteSummaryFirstname' -> athleteSummary { athleteSummaryFirstname = athleteSummaryFirstname' }
    )

class FlaggedLens a b | a -> b where
  flagged :: Lens a b
instance FlaggedLens ActivityDetailed Bool where
  flagged activityDetailed =
    ( activityDetailedFlagged activityDetailed
    , \ activityDetailedFlagged' -> activityDetailed { activityDetailedFlagged = activityDetailedFlagged' }
    )
instance FlaggedLens ActivitySummary Bool where
  flagged activitySummary =
    ( activitySummaryFlagged activitySummary
    , \ activitySummaryFlagged' -> activitySummary { activitySummaryFlagged = activitySummaryFlagged' }
    )

class FollowerCountLens a b | a -> b where
  followerCount :: Lens a b
instance FollowerCountLens AthleteDetailed Integer where
  followerCount athleteDetailed =
    ( athleteDetailedFollowerCount athleteDetailed
    , \ athleteDetailedFollowerCount' -> athleteDetailed { athleteDetailedFollowerCount = athleteDetailedFollowerCount' }
    )

class FollowerLens a b | a -> b where
  follower :: Lens a b
instance FollowerLens AthleteDetailed (Maybe Text) where
  follower athleteDetailed =
    ( athleteDetailedFollower athleteDetailed
    , \ athleteDetailedFollower' -> athleteDetailed { athleteDetailedFollower = athleteDetailedFollower' }
    )
instance FollowerLens AthleteSummary (Maybe Text) where
  follower athleteSummary =
    ( athleteSummaryFollower athleteSummary
    , \ athleteSummaryFollower' -> athleteSummary { athleteSummaryFollower = athleteSummaryFollower' }
    )

class FrameTypeLens a b | a -> b where
  frameType :: Lens a b
instance FrameTypeLens GearDetailed (Maybe Integer) where
  frameType gearDetailed =
    ( gearDetailedFrameType gearDetailed
    , \ gearDetailedFrameType' -> gearDetailed { gearDetailedFrameType = gearDetailedFrameType' }
    )

class FriendCountLens a b | a -> b where
  friendCount :: Lens a b
instance FriendCountLens AthleteDetailed Integer where
  friendCount athleteDetailed =
    ( athleteDetailedFriendCount athleteDetailed
    , \ athleteDetailedFriendCount' -> athleteDetailed { athleteDetailedFriendCount = athleteDetailedFriendCount' }
    )

class FriendLens a b | a -> b where
  friend :: Lens a b
instance FriendLens AthleteDetailed (Maybe Text) where
  friend athleteDetailed =
    ( athleteDetailedFriend athleteDetailed
    , \ athleteDetailedFriend' -> athleteDetailed { athleteDetailedFriend = athleteDetailedFriend' }
    )
instance FriendLens AthleteSummary (Maybe Text) where
  friend athleteSummary =
    ( athleteSummaryFriend athleteSummary
    , \ athleteSummaryFriend' -> athleteSummary { athleteSummaryFriend = athleteSummaryFriend' }
    )

class FtpLens a b | a -> b where
  ftp :: Lens a b
instance FtpLens AthleteDetailed (Maybe Integer) where
  ftp athleteDetailed =
    ( athleteDetailedFtp athleteDetailed
    , \ athleteDetailedFtp' -> athleteDetailed { athleteDetailedFtp = athleteDetailedFtp' }
    )

class GearIdLens a b | a -> b where
  gearId :: Lens a b
instance GearIdLens ActivityDetailed (Maybe Text) where
  gearId activityDetailed =
    ( activityDetailedGearId activityDetailed
    , \ activityDetailedGearId' -> activityDetailed { activityDetailedGearId = activityDetailedGearId' }
    )
instance GearIdLens ActivitySummary (Maybe Text) where
  gearId activitySummary =
    ( activitySummaryGearId activitySummary
    , \ activitySummaryGearId' -> activitySummary { activitySummaryGearId = activitySummaryGearId' }
    )

class GearLens a b | a -> b where
  gear :: Lens a b
instance GearLens ActivityDetailed GearSummary where
  gear activityDetailed =
    ( activityDetailedGear activityDetailed
    , \ activityDetailedGear' -> activityDetailed { activityDetailedGear = activityDetailedGear' }
    )

class HasKudoedLens a b | a -> b where
  hasKudoed :: Lens a b
instance HasKudoedLens ActivityDetailed Bool where
  hasKudoed activityDetailed =
    ( activityDetailedHasKudoed activityDetailed
    , \ activityDetailedHasKudoed' -> activityDetailed { activityDetailedHasKudoed = activityDetailedHasKudoed' }
    )
instance HasKudoedLens ActivitySummary Bool where
  hasKudoed activitySummary =
    ( activitySummaryHasKudoed activitySummary
    , \ activitySummaryHasKudoed' -> activitySummary { activitySummaryHasKudoed = activitySummaryHasKudoed' }
    )

class HazardousLens a b | a -> b where
  hazardous :: Lens a b
instance HazardousLens SegmentDetailed Bool where
  hazardous segmentDetailed =
    ( segmentDetailedHazardous segmentDetailed
    , \ segmentDetailedHazardous' -> segmentDetailed { segmentDetailedHazardous = segmentDetailedHazardous' }
    )

class HiddenLens a b | a -> b where
  hidden :: Lens a b
instance HiddenLens EffortDetailed (Maybe Bool) where
  hidden effortDetailed =
    ( effortDetailedHidden effortDetailed
    , \ effortDetailedHidden' -> effortDetailed { effortDetailedHidden = effortDetailedHidden' }
    )

class IdLens a b | a -> b where
  id :: Lens a b
instance IdLens ActivityDetailed Integer where
  id activityDetailed =
    ( activityDetailedId activityDetailed
    , \ activityDetailedId' -> activityDetailed { activityDetailedId = activityDetailedId' }
    )
instance IdLens ActivitySummary Integer where
  id activitySummary =
    ( activitySummaryId activitySummary
    , \ activitySummaryId' -> activitySummary { activitySummaryId = activitySummaryId' }
    )
instance IdLens ActivityLapSummary Integer where
  id activityLapSummary =
    ( activityLapSummaryId activityLapSummary
    , \ activityLapSummaryId' -> activityLapSummary { activityLapSummaryId = activityLapSummaryId' }
    )
instance IdLens AthleteDetailed Integer where
  id athleteDetailed =
    ( athleteDetailedId athleteDetailed
    , \ athleteDetailedId' -> athleteDetailed { athleteDetailedId = athleteDetailedId' }
    )
instance IdLens AthleteSummary Integer where
  id athleteSummary =
    ( athleteSummaryId athleteSummary
    , \ athleteSummaryId' -> athleteSummary { athleteSummaryId = athleteSummaryId' }
    )
instance IdLens AthleteMeta Integer where
  id athleteMeta =
    ( athleteMetaId athleteMeta
    , \ athleteMetaId' -> athleteMeta { athleteMetaId = athleteMetaId' }
    )
instance IdLens ClubDetailed Integer where
  id clubDetailed =
    ( clubDetailedId clubDetailed
    , \ clubDetailedId' -> clubDetailed { clubDetailedId = clubDetailedId' }
    )
instance IdLens ClubSummary Integer where
  id clubSummary =
    ( clubSummaryId clubSummary
    , \ clubSummaryId' -> clubSummary { clubSummaryId = clubSummaryId' }
    )
instance IdLens CommentSummary Integer where
  id commentSummary =
    ( commentSummaryId commentSummary
    , \ commentSummaryId' -> commentSummary { commentSummaryId = commentSummaryId' }
    )
instance IdLens EffortDetailed Integer where
  id effortDetailed =
    ( effortDetailedId effortDetailed
    , \ effortDetailedId' -> effortDetailed { effortDetailedId = effortDetailedId' }
    )
instance IdLens GearDetailed Text where
  id gearDetailed =
    ( gearDetailedId gearDetailed
    , \ gearDetailedId' -> gearDetailed { gearDetailedId = gearDetailedId' }
    )
instance IdLens GearSummary Text where
  id gearSummary =
    ( gearSummaryId gearSummary
    , \ gearSummaryId' -> gearSummary { gearSummaryId = gearSummaryId' }
    )
instance IdLens PhotoSummary Integer where
  id photoSummary =
    ( photoSummaryId photoSummary
    , \ photoSummaryId' -> photoSummary { photoSummaryId = photoSummaryId' }
    )
instance IdLens PolylineDetailed Text where
  id polylineDetailed =
    ( polylineDetailedId polylineDetailed
    , \ polylineDetailedId' -> polylineDetailed { polylineDetailedId = polylineDetailedId' }
    )
instance IdLens PolylineSummary Text where
  id polylineSummary =
    ( polylineSummaryId polylineSummary
    , \ polylineSummaryId' -> polylineSummary { polylineSummaryId = polylineSummaryId' }
    )
instance IdLens SegmentDetailed Integer where
  id segmentDetailed =
    ( segmentDetailedId segmentDetailed
    , \ segmentDetailedId' -> segmentDetailed { segmentDetailedId = segmentDetailedId' }
    )
instance IdLens SegmentSummary Integer where
  id segmentSummary =
    ( segmentSummaryId segmentSummary
    , \ segmentSummaryId' -> segmentSummary { segmentSummaryId = segmentSummaryId' }
    )
instance IdLens SegmentExplorerEntry Integer where
  id segmentExplorerEntry =
    ( segmentExplorerEntryId segmentExplorerEntry
    , \ segmentExplorerEntryId' -> segmentExplorerEntry { segmentExplorerEntryId = segmentExplorerEntryId' }
    )
instance IdLens UploadStatus Integer where
  id uploadStatus =
    ( uploadStatusId uploadStatus
    , \ uploadStatusId' -> uploadStatus { uploadStatusId = uploadStatusId' }
    )

class InstagramPrimaryPhotoLens a b | a -> b where
  instagramPrimaryPhoto :: Lens a b
instance InstagramPrimaryPhotoLens ActivityDetailed Text where
  instagramPrimaryPhoto activityDetailed =
    ( activityDetailedInstagramPrimaryPhoto activityDetailed
    , \ activityDetailedInstagramPrimaryPhoto' -> activityDetailed { activityDetailedInstagramPrimaryPhoto = activityDetailedInstagramPrimaryPhoto' }
    )

class KilojoulesLens a b | a -> b where
  kilojoules :: Lens a b
instance KilojoulesLens ActivityDetailed (Maybe Double) where
  kilojoules activityDetailed =
    ( activityDetailedKilojoules activityDetailed
    , \ activityDetailedKilojoules' -> activityDetailed { activityDetailedKilojoules = activityDetailedKilojoules' }
    )
instance KilojoulesLens ActivitySummary (Maybe Double) where
  kilojoules activitySummary =
    ( activitySummaryKilojoules activitySummary
    , \ activitySummaryKilojoules' -> activitySummary { activitySummaryKilojoules = activitySummaryKilojoules' }
    )

class KomRankLens a b | a -> b where
  komRank :: Lens a b
instance KomRankLens EffortDetailed (Maybe Integer) where
  komRank effortDetailed =
    ( effortDetailedKomRank effortDetailed
    , \ effortDetailedKomRank' -> effortDetailed { effortDetailedKomRank = effortDetailedKomRank' }
    )

class KudosCountLens a b | a -> b where
  kudosCount :: Lens a b
instance KudosCountLens ActivitySummary Integer where
  kudosCount activitySummary =
    ( activitySummaryKudosCount activitySummary
    , \ activitySummaryKudosCount' -> activitySummary { activitySummaryKudosCount = activitySummaryKudosCount' }
    )

class LapIndexLens a b | a -> b where
  lapIndex :: Lens a b
instance LapIndexLens ActivityLapSummary Integer where
  lapIndex activityLapSummary =
    ( activityLapSummaryLapIndex activityLapSummary
    , \ activityLapSummaryLapIndex' -> activityLapSummary { activityLapSummaryLapIndex = activityLapSummaryLapIndex' }
    )

class LastnameLens a b | a -> b where
  lastname :: Lens a b
instance LastnameLens AthleteDetailed Text where
  lastname athleteDetailed =
    ( athleteDetailedLastname athleteDetailed
    , \ athleteDetailedLastname' -> athleteDetailed { athleteDetailedLastname = athleteDetailedLastname' }
    )
instance LastnameLens AthleteSummary Text where
  lastname athleteSummary =
    ( athleteSummaryLastname athleteSummary
    , \ athleteSummaryLastname' -> athleteSummary { athleteSummaryLastname = athleteSummaryLastname' }
    )

class LocationCityLens a b | a -> b where
  locationCity :: Lens a b
instance LocationCityLens ActivityDetailed (Maybe Text) where
  locationCity activityDetailed =
    ( activityDetailedLocationCity activityDetailed
    , \ activityDetailedLocationCity' -> activityDetailed { activityDetailedLocationCity = activityDetailedLocationCity' }
    )
instance LocationCityLens ActivitySummary (Maybe Text) where
  locationCity activitySummary =
    ( activitySummaryLocationCity activitySummary
    , \ activitySummaryLocationCity' -> activitySummary { activitySummaryLocationCity = activitySummaryLocationCity' }
    )

class LocationCountryLens a b | a -> b where
  locationCountry :: Lens a b
instance LocationCountryLens ActivityDetailed Text where
  locationCountry activityDetailed =
    ( activityDetailedLocationCountry activityDetailed
    , \ activityDetailedLocationCountry' -> activityDetailed { activityDetailedLocationCountry = activityDetailedLocationCountry' }
    )
instance LocationCountryLens ActivitySummary Text where
  locationCountry activitySummary =
    ( activitySummaryLocationCountry activitySummary
    , \ activitySummaryLocationCountry' -> activitySummary { activitySummaryLocationCountry = activitySummaryLocationCountry' }
    )

class LocationLens a b | a -> b where
  location :: Lens a b
instance LocationLens PhotoSummary (Maybe (Double, Double)) where
  location photoSummary =
    ( photoSummaryLocation photoSummary
    , \ photoSummaryLocation' -> photoSummary { photoSummaryLocation = photoSummaryLocation' }
    )

class LocationStateLens a b | a -> b where
  locationState :: Lens a b
instance LocationStateLens ActivityDetailed (Maybe Text) where
  locationState activityDetailed =
    ( activityDetailedLocationState activityDetailed
    , \ activityDetailedLocationState' -> activityDetailed { activityDetailedLocationState = activityDetailedLocationState' }
    )
instance LocationStateLens ActivitySummary (Maybe Text) where
  locationState activitySummary =
    ( activitySummaryLocationState activitySummary
    , \ activitySummaryLocationState' -> activitySummary { activitySummaryLocationState = activitySummaryLocationState' }
    )

class ManualLens a b | a -> b where
  manual :: Lens a b
instance ManualLens ActivityDetailed Bool where
  manual activityDetailed =
    ( activityDetailedManual activityDetailed
    , \ activityDetailedManual' -> activityDetailed { activityDetailedManual = activityDetailedManual' }
    )
instance ManualLens ActivitySummary Bool where
  manual activitySummary =
    ( activitySummaryManual activitySummary
    , \ activitySummaryManual' -> activitySummary { activitySummaryManual = activitySummaryManual' }
    )

class MapLens a b | a -> b where
  map :: Lens a b
instance MapLens ActivityDetailed PolylineDetailed where
  map activityDetailed =
    ( activityDetailedMap activityDetailed
    , \ activityDetailedMap' -> activityDetailed { activityDetailedMap = activityDetailedMap' }
    )
instance MapLens ActivitySummary PolylineSummary where
  map activitySummary =
    ( activitySummaryMap activitySummary
    , \ activitySummaryMap' -> activitySummary { activitySummaryMap = activitySummaryMap' }
    )
instance MapLens SegmentDetailed PolylineDetailed where
  map segmentDetailed =
    ( segmentDetailedMap segmentDetailed
    , \ segmentDetailedMap' -> segmentDetailed { segmentDetailedMap = segmentDetailedMap' }
    )

class MaxHeartrateLens a b | a -> b where
  maxHeartrate :: Lens a b
instance MaxHeartrateLens EffortDetailed (Maybe Integer) where
  maxHeartrate effortDetailed =
    ( effortDetailedMaxHeartrate effortDetailed
    , \ effortDetailedMaxHeartrate' -> effortDetailed { effortDetailedMaxHeartrate = effortDetailedMaxHeartrate' }
    )

class MaxLens a b | a -> b where
  max :: Lens a b
instance MaxLens ActivityZoneDistributionBucket Integer where
  max activityZoneDistributionBucket =
    ( activityZoneDistributionBucketMax activityZoneDistributionBucket
    , \ activityZoneDistributionBucketMax' -> activityZoneDistributionBucket { activityZoneDistributionBucketMax = activityZoneDistributionBucketMax' }
    )

class MaxSpeedLens a b | a -> b where
  maxSpeed :: Lens a b
instance MaxSpeedLens ActivityDetailed Double where
  maxSpeed activityDetailed =
    ( activityDetailedMaxSpeed activityDetailed
    , \ activityDetailedMaxSpeed' -> activityDetailed { activityDetailedMaxSpeed = activityDetailedMaxSpeed' }
    )
instance MaxSpeedLens ActivitySummary Double where
  maxSpeed activitySummary =
    ( activitySummaryMaxSpeed activitySummary
    , \ activitySummaryMaxSpeed' -> activitySummary { activitySummaryMaxSpeed = activitySummaryMaxSpeed' }
    )
instance MaxSpeedLens ActivityLapSummary Double where
  maxSpeed activityLapSummary =
    ( activityLapSummaryMaxSpeed activityLapSummary
    , \ activityLapSummaryMaxSpeed' -> activityLapSummary { activityLapSummaryMaxSpeed = activityLapSummaryMaxSpeed' }
    )

class MaximumGradeLens a b | a -> b where
  maximumGrade :: Lens a b
instance MaximumGradeLens SegmentDetailed Double where
  maximumGrade segmentDetailed =
    ( segmentDetailedMaximumGrade segmentDetailed
    , \ segmentDetailedMaximumGrade' -> segmentDetailed { segmentDetailedMaximumGrade = segmentDetailedMaximumGrade' }
    )
instance MaximumGradeLens SegmentSummary Double where
  maximumGrade segmentSummary =
    ( segmentSummaryMaximumGrade segmentSummary
    , \ segmentSummaryMaximumGrade' -> segmentSummary { segmentSummaryMaximumGrade = segmentSummaryMaximumGrade' }
    )

class MeasurementPreferenceLens a b | a -> b where
  measurementPreference :: Lens a b
instance MeasurementPreferenceLens AthleteDetailed Text where
  measurementPreference athleteDetailed =
    ( athleteDetailedMeasurementPreference athleteDetailed
    , \ athleteDetailedMeasurementPreference' -> athleteDetailed { athleteDetailedMeasurementPreference = athleteDetailedMeasurementPreference' }
    )

class MemberCountLens a b | a -> b where
  memberCount :: Lens a b
instance MemberCountLens ClubDetailed Integer where
  memberCount clubDetailed =
    ( clubDetailedMemberCount clubDetailed
    , \ clubDetailedMemberCount' -> clubDetailed { clubDetailedMemberCount = clubDetailedMemberCount' }
    )

class MinLens a b | a -> b where
  min :: Lens a b
instance MinLens ActivityZoneDistributionBucket Integer where
  min activityZoneDistributionBucket =
    ( activityZoneDistributionBucketMin activityZoneDistributionBucket
    , \ activityZoneDistributionBucketMin' -> activityZoneDistributionBucket { activityZoneDistributionBucketMin = activityZoneDistributionBucketMin' }
    )

class ModelNameLens a b | a -> b where
  modelName :: Lens a b
instance ModelNameLens GearDetailed Text where
  modelName gearDetailed =
    ( gearDetailedModelName gearDetailed
    , \ gearDetailedModelName' -> gearDetailed { gearDetailedModelName = gearDetailedModelName' }
    )

class MovingTimeLens a b | a -> b where
  movingTime :: Lens a b
instance MovingTimeLens ActivityDetailed Integer where
  movingTime activityDetailed =
    ( activityDetailedMovingTime activityDetailed
    , \ activityDetailedMovingTime' -> activityDetailed { activityDetailedMovingTime = activityDetailedMovingTime' }
    )
instance MovingTimeLens ActivitySummary Integer where
  movingTime activitySummary =
    ( activitySummaryMovingTime activitySummary
    , \ activitySummaryMovingTime' -> activitySummary { activitySummaryMovingTime = activitySummaryMovingTime' }
    )
instance MovingTimeLens ActivityLapSummary Double where
  movingTime activityLapSummary =
    ( activityLapSummaryMovingTime activityLapSummary
    , \ activityLapSummaryMovingTime' -> activityLapSummary { activityLapSummaryMovingTime = activityLapSummaryMovingTime' }
    )
instance MovingTimeLens EffortDetailed Integer where
  movingTime effortDetailed =
    ( effortDetailedMovingTime effortDetailed
    , \ effortDetailedMovingTime' -> effortDetailed { effortDetailedMovingTime = effortDetailedMovingTime' }
    )
instance MovingTimeLens SegmentLeaderboardEntry Integer where
  movingTime segmentLeaderboardEntry =
    ( segmentLeaderboardEntryMovingTime segmentLeaderboardEntry
    , \ segmentLeaderboardEntryMovingTime' -> segmentLeaderboardEntry { segmentLeaderboardEntryMovingTime = segmentLeaderboardEntryMovingTime' }
    )

class MutualFriendCountLens a b | a -> b where
  mutualFriendCount :: Lens a b
instance MutualFriendCountLens AthleteDetailed Integer where
  mutualFriendCount athleteDetailed =
    ( athleteDetailedMutualFriendCount athleteDetailed
    , \ athleteDetailedMutualFriendCount' -> athleteDetailed { athleteDetailedMutualFriendCount = athleteDetailedMutualFriendCount' }
    )

class NameLens a b | a -> b where
  name :: Lens a b
instance NameLens ActivityDetailed Text where
  name activityDetailed =
    ( activityDetailedName activityDetailed
    , \ activityDetailedName' -> activityDetailed { activityDetailedName = activityDetailedName' }
    )
instance NameLens ActivitySummary Text where
  name activitySummary =
    ( activitySummaryName activitySummary
    , \ activitySummaryName' -> activitySummary { activitySummaryName = activitySummaryName' }
    )
instance NameLens ActivityLapSummary Text where
  name activityLapSummary =
    ( activityLapSummaryName activityLapSummary
    , \ activityLapSummaryName' -> activityLapSummary { activityLapSummaryName = activityLapSummaryName' }
    )
instance NameLens ClubDetailed Text where
  name clubDetailed =
    ( clubDetailedName clubDetailed
    , \ clubDetailedName' -> clubDetailed { clubDetailedName = clubDetailedName' }
    )
instance NameLens ClubSummary Text where
  name clubSummary =
    ( clubSummaryName clubSummary
    , \ clubSummaryName' -> clubSummary { clubSummaryName = clubSummaryName' }
    )
instance NameLens EffortDetailed Text where
  name effortDetailed =
    ( effortDetailedName effortDetailed
    , \ effortDetailedName' -> effortDetailed { effortDetailedName = effortDetailedName' }
    )
instance NameLens GearDetailed Text where
  name gearDetailed =
    ( gearDetailedName gearDetailed
    , \ gearDetailedName' -> gearDetailed { gearDetailedName = gearDetailedName' }
    )
instance NameLens GearSummary Text where
  name gearSummary =
    ( gearSummaryName gearSummary
    , \ gearSummaryName' -> gearSummary { gearSummaryName = gearSummaryName' }
    )
instance NameLens SegmentDetailed Text where
  name segmentDetailed =
    ( segmentDetailedName segmentDetailed
    , \ segmentDetailedName' -> segmentDetailed { segmentDetailedName = segmentDetailedName' }
    )
instance NameLens SegmentSummary Text where
  name segmentSummary =
    ( segmentSummaryName segmentSummary
    , \ segmentSummaryName' -> segmentSummary { segmentSummaryName = segmentSummaryName' }
    )
instance NameLens SegmentExplorerEntry Text where
  name segmentExplorerEntry =
    ( segmentExplorerEntryName segmentExplorerEntry
    , \ segmentExplorerEntryName' -> segmentExplorerEntry { segmentExplorerEntryName = segmentExplorerEntryName' }
    )

class OriginalSizeLens a b | a -> b where
  originalSize :: Lens a b
instance OriginalSizeLens StreamDetailed Integer where
  originalSize streamDetailed =
    ( streamDetailedOriginalSize streamDetailed
    , \ streamDetailedOriginalSize' -> streamDetailed { streamDetailedOriginalSize = streamDetailedOriginalSize' }
    )

class PhotoCountLens a b | a -> b where
  photoCount :: Lens a b
instance PhotoCountLens ActivityDetailed Integer where
  photoCount activityDetailed =
    ( activityDetailedPhotoCount activityDetailed
    , \ activityDetailedPhotoCount' -> activityDetailed { activityDetailedPhotoCount = activityDetailedPhotoCount' }
    )
instance PhotoCountLens ActivitySummary Integer where
  photoCount activitySummary =
    ( activitySummaryPhotoCount activitySummary
    , \ activitySummaryPhotoCount' -> activitySummary { activitySummaryPhotoCount = activitySummaryPhotoCount' }
    )

class PointsLens a b | a -> b where
  points :: Lens a b
instance PointsLens SegmentExplorerEntry Text where
  points segmentExplorerEntry =
    ( segmentExplorerEntryPoints segmentExplorerEntry
    , \ segmentExplorerEntryPoints' -> segmentExplorerEntry { segmentExplorerEntryPoints = segmentExplorerEntryPoints' }
    )

class PolylineLens a b | a -> b where
  polyline :: Lens a b
instance PolylineLens PolylineDetailed ([(Double, Double)]) where
  polyline polylineDetailed =
    ( polylineDetailedPolyline polylineDetailed
    , \ polylineDetailedPolyline' -> polylineDetailed { polylineDetailedPolyline = polylineDetailedPolyline' }
    )

class PrRankLens a b | a -> b where
  prRank :: Lens a b
instance PrRankLens EffortDetailed (Maybe Integer) where
  prRank effortDetailed =
    ( effortDetailedPrRank effortDetailed
    , \ effortDetailedPrRank' -> effortDetailed { effortDetailedPrRank = effortDetailedPrRank' }
    )

class PremiumLens a b | a -> b where
  premium :: Lens a b
instance PremiumLens AthleteDetailed Bool where
  premium athleteDetailed =
    ( athleteDetailedPremium athleteDetailed
    , \ athleteDetailedPremium' -> athleteDetailed { athleteDetailedPremium = athleteDetailedPremium' }
    )
instance PremiumLens AthleteSummary Bool where
  premium athleteSummary =
    ( athleteSummaryPremium athleteSummary
    , \ athleteSummaryPremium' -> athleteSummary { athleteSummaryPremium = athleteSummaryPremium' }
    )

class PrimaryLens a b | a -> b where
  primary :: Lens a b
instance PrimaryLens GearDetailed Bool where
  primary gearDetailed =
    ( gearDetailedPrimary gearDetailed
    , \ gearDetailedPrimary' -> gearDetailed { gearDetailedPrimary = gearDetailedPrimary' }
    )
instance PrimaryLens GearSummary Bool where
  primary gearSummary =
    ( gearSummaryPrimary gearSummary
    , \ gearSummaryPrimary' -> gearSummary { gearSummaryPrimary = gearSummaryPrimary' }
    )

class PrivateLens a b | a -> b where
  private :: Lens a b
instance PrivateLens ActivityDetailed Bool where
  private activityDetailed =
    ( activityDetailedPrivate activityDetailed
    , \ activityDetailedPrivate' -> activityDetailed { activityDetailedPrivate = activityDetailedPrivate' }
    )
instance PrivateLens ActivitySummary Bool where
  private activitySummary =
    ( activitySummaryPrivate activitySummary
    , \ activitySummaryPrivate' -> activitySummary { activitySummaryPrivate = activitySummaryPrivate' }
    )
instance PrivateLens ClubDetailed Bool where
  private clubDetailed =
    ( clubDetailedPrivate clubDetailed
    , \ clubDetailedPrivate' -> clubDetailed { clubDetailedPrivate = clubDetailedPrivate' }
    )
instance PrivateLens SegmentDetailed Bool where
  private segmentDetailed =
    ( segmentDetailedPrivate segmentDetailed
    , \ segmentDetailedPrivate' -> segmentDetailed { segmentDetailedPrivate = segmentDetailedPrivate' }
    )
instance PrivateLens SegmentSummary Bool where
  private segmentSummary =
    ( segmentSummaryPrivate segmentSummary
    , \ segmentSummaryPrivate' -> segmentSummary { segmentSummaryPrivate = segmentSummaryPrivate' }
    )

class ProfileLens a b | a -> b where
  profile :: Lens a b
instance ProfileLens AthleteDetailed Text where
  profile athleteDetailed =
    ( athleteDetailedProfile athleteDetailed
    , \ athleteDetailedProfile' -> athleteDetailed { athleteDetailedProfile = athleteDetailedProfile' }
    )
instance ProfileLens AthleteSummary Text where
  profile athleteSummary =
    ( athleteSummaryProfile athleteSummary
    , \ athleteSummaryProfile' -> athleteSummary { athleteSummaryProfile = athleteSummaryProfile' }
    )
instance ProfileLens ClubDetailed Text where
  profile clubDetailed =
    ( clubDetailedProfile clubDetailed
    , \ clubDetailedProfile' -> clubDetailed { clubDetailedProfile = clubDetailedProfile' }
    )
instance ProfileLens ClubSummary Text where
  profile clubSummary =
    ( clubSummaryProfile clubSummary
    , \ clubSummaryProfile' -> clubSummary { clubSummaryProfile = clubSummaryProfile' }
    )

class ProfileMediumLens a b | a -> b where
  profileMedium :: Lens a b
instance ProfileMediumLens AthleteDetailed Text where
  profileMedium athleteDetailed =
    ( athleteDetailedProfileMedium athleteDetailed
    , \ athleteDetailedProfileMedium' -> athleteDetailed { athleteDetailedProfileMedium = athleteDetailedProfileMedium' }
    )
instance ProfileMediumLens AthleteSummary Text where
  profileMedium athleteSummary =
    ( athleteSummaryProfileMedium athleteSummary
    , \ athleteSummaryProfileMedium' -> athleteSummary { athleteSummaryProfileMedium = athleteSummaryProfileMedium' }
    )
instance ProfileMediumLens ClubDetailed Text where
  profileMedium clubDetailed =
    ( clubDetailedProfileMedium clubDetailed
    , \ clubDetailedProfileMedium' -> clubDetailed { clubDetailedProfileMedium = clubDetailedProfileMedium' }
    )
instance ProfileMediumLens ClubSummary Text where
  profileMedium clubSummary =
    ( clubSummaryProfileMedium clubSummary
    , \ clubSummaryProfileMedium' -> clubSummary { clubSummaryProfileMedium = clubSummaryProfileMedium' }
    )

class RankLens a b | a -> b where
  rank :: Lens a b
instance RankLens SegmentLeaderboardEntry Integer where
  rank segmentLeaderboardEntry =
    ( segmentLeaderboardEntryRank segmentLeaderboardEntry
    , \ segmentLeaderboardEntryRank' -> segmentLeaderboardEntry { segmentLeaderboardEntryRank = segmentLeaderboardEntryRank' }
    )

class RefLens a b | a -> b where
  ref :: Lens a b
instance RefLens PhotoSummary Text where
  ref photoSummary =
    ( photoSummaryRef photoSummary
    , \ photoSummaryRef' -> photoSummary { photoSummaryRef = photoSummaryRef' }
    )

class ResolutionLens a b | a -> b where
  resolution :: Lens a b
instance ResolutionLens StreamDetailed Text where
  resolution streamDetailed =
    ( streamDetailedResolution streamDetailed
    , \ streamDetailedResolution' -> streamDetailed { streamDetailedResolution = streamDetailedResolution' }
    )

class ResourceStateLens a b | a -> b where
  resourceState :: Lens a b
instance ResourceStateLens ActivityDetailed Integer where
  resourceState activityDetailed =
    ( activityDetailedResourceState activityDetailed
    , \ activityDetailedResourceState' -> activityDetailed { activityDetailedResourceState = activityDetailedResourceState' }
    )
instance ResourceStateLens ActivitySummary Integer where
  resourceState activitySummary =
    ( activitySummaryResourceState activitySummary
    , \ activitySummaryResourceState' -> activitySummary { activitySummaryResourceState = activitySummaryResourceState' }
    )
instance ResourceStateLens ActivityZoneDetailed Integer where
  resourceState activityZoneDetailed =
    ( activityZoneDetailedResourceState activityZoneDetailed
    , \ activityZoneDetailedResourceState' -> activityZoneDetailed { activityZoneDetailedResourceState = activityZoneDetailedResourceState' }
    )
instance ResourceStateLens ActivityLapSummary Integer where
  resourceState activityLapSummary =
    ( activityLapSummaryResourceState activityLapSummary
    , \ activityLapSummaryResourceState' -> activityLapSummary { activityLapSummaryResourceState = activityLapSummaryResourceState' }
    )
instance ResourceStateLens AthleteDetailed Integer where
  resourceState athleteDetailed =
    ( athleteDetailedResourceState athleteDetailed
    , \ athleteDetailedResourceState' -> athleteDetailed { athleteDetailedResourceState = athleteDetailedResourceState' }
    )
instance ResourceStateLens AthleteSummary Integer where
  resourceState athleteSummary =
    ( athleteSummaryResourceState athleteSummary
    , \ athleteSummaryResourceState' -> athleteSummary { athleteSummaryResourceState = athleteSummaryResourceState' }
    )
instance ResourceStateLens AthleteMeta Integer where
  resourceState athleteMeta =
    ( athleteMetaResourceState athleteMeta
    , \ athleteMetaResourceState' -> athleteMeta { athleteMetaResourceState = athleteMetaResourceState' }
    )
instance ResourceStateLens ClubDetailed Integer where
  resourceState clubDetailed =
    ( clubDetailedResourceState clubDetailed
    , \ clubDetailedResourceState' -> clubDetailed { clubDetailedResourceState = clubDetailedResourceState' }
    )
instance ResourceStateLens ClubSummary Integer where
  resourceState clubSummary =
    ( clubSummaryResourceState clubSummary
    , \ clubSummaryResourceState' -> clubSummary { clubSummaryResourceState = clubSummaryResourceState' }
    )
instance ResourceStateLens CommentSummary Integer where
  resourceState commentSummary =
    ( commentSummaryResourceState commentSummary
    , \ commentSummaryResourceState' -> commentSummary { commentSummaryResourceState = commentSummaryResourceState' }
    )
instance ResourceStateLens EffortDetailed Integer where
  resourceState effortDetailed =
    ( effortDetailedResourceState effortDetailed
    , \ effortDetailedResourceState' -> effortDetailed { effortDetailedResourceState = effortDetailedResourceState' }
    )
instance ResourceStateLens GearDetailed Integer where
  resourceState gearDetailed =
    ( gearDetailedResourceState gearDetailed
    , \ gearDetailedResourceState' -> gearDetailed { gearDetailedResourceState = gearDetailedResourceState' }
    )
instance ResourceStateLens GearSummary Integer where
  resourceState gearSummary =
    ( gearSummaryResourceState gearSummary
    , \ gearSummaryResourceState' -> gearSummary { gearSummaryResourceState = gearSummaryResourceState' }
    )
instance ResourceStateLens PhotoSummary Integer where
  resourceState photoSummary =
    ( photoSummaryResourceState photoSummary
    , \ photoSummaryResourceState' -> photoSummary { photoSummaryResourceState = photoSummaryResourceState' }
    )
instance ResourceStateLens PolylineDetailed Integer where
  resourceState polylineDetailed =
    ( polylineDetailedResourceState polylineDetailed
    , \ polylineDetailedResourceState' -> polylineDetailed { polylineDetailedResourceState = polylineDetailedResourceState' }
    )
instance ResourceStateLens PolylineSummary Integer where
  resourceState polylineSummary =
    ( polylineSummaryResourceState polylineSummary
    , \ polylineSummaryResourceState' -> polylineSummary { polylineSummaryResourceState = polylineSummaryResourceState' }
    )
instance ResourceStateLens SegmentDetailed Integer where
  resourceState segmentDetailed =
    ( segmentDetailedResourceState segmentDetailed
    , \ segmentDetailedResourceState' -> segmentDetailed { segmentDetailedResourceState = segmentDetailedResourceState' }
    )
instance ResourceStateLens SegmentSummary Integer where
  resourceState segmentSummary =
    ( segmentSummaryResourceState segmentSummary
    , \ segmentSummaryResourceState' -> segmentSummary { segmentSummaryResourceState = segmentSummaryResourceState' }
    )
instance ResourceStateLens SegmentExplorerEntry Integer where
  resourceState segmentExplorerEntry =
    ( segmentExplorerEntryResourceState segmentExplorerEntry
    , \ segmentExplorerEntryResourceState' -> segmentExplorerEntry { segmentExplorerEntryResourceState = segmentExplorerEntryResourceState' }
    )

class SegmentEffortsLens a b | a -> b where
  segmentEfforts :: Lens a b
instance SegmentEffortsLens ActivityDetailed [EffortDetailed] where
  segmentEfforts activityDetailed =
    ( activityDetailedSegmentEfforts activityDetailed
    , \ activityDetailedSegmentEfforts' -> activityDetailed { activityDetailedSegmentEfforts = activityDetailedSegmentEfforts' }
    )

class SegmentLens a b | a -> b where
  segment :: Lens a b
instance SegmentLens EffortDetailed SegmentSummary where
  segment effortDetailed =
    ( effortDetailedSegment effortDetailed
    , \ effortDetailedSegment' -> effortDetailed { effortDetailedSegment = effortDetailedSegment' }
    )

class SensorBasedLens a b | a -> b where
  sensorBased :: Lens a b
instance SensorBasedLens ActivityZoneDetailed Bool where
  sensorBased activityZoneDetailed =
    ( activityZoneDetailedSensorBased activityZoneDetailed
    , \ activityZoneDetailedSensorBased' -> activityZoneDetailed { activityZoneDetailedSensorBased = activityZoneDetailedSensorBased' }
    )

class SeriesTypeLens a b | a -> b where
  seriesType :: Lens a b
instance SeriesTypeLens StreamDetailed Text where
  seriesType streamDetailed =
    ( streamDetailedSeriesType streamDetailed
    , \ streamDetailedSeriesType' -> streamDetailed { streamDetailedSeriesType = streamDetailedSeriesType' }
    )

class SexLens a b | a -> b where
  sex :: Lens a b
instance SexLens AthleteDetailed (Maybe Char) where
  sex athleteDetailed =
    ( athleteDetailedSex athleteDetailed
    , \ athleteDetailedSex' -> athleteDetailed { athleteDetailedSex = athleteDetailedSex' }
    )
instance SexLens AthleteSummary (Maybe Char) where
  sex athleteSummary =
    ( athleteSummarySex athleteSummary
    , \ athleteSummarySex' -> athleteSummary { athleteSummarySex = athleteSummarySex' }
    )

class ShoesLens a b | a -> b where
  shoes :: Lens a b
instance ShoesLens AthleteDetailed [GearSummary] where
  shoes athleteDetailed =
    ( athleteDetailedShoes athleteDetailed
    , \ athleteDetailedShoes' -> athleteDetailed { athleteDetailedShoes = athleteDetailedShoes' }
    )

class SportTypeLens a b | a -> b where
  sportType :: Lens a b
instance SportTypeLens ClubDetailed Text where
  sportType clubDetailed =
    ( clubDetailedSportType clubDetailed
    , \ clubDetailedSportType' -> clubDetailed { clubDetailedSportType = clubDetailedSportType' }
    )

class StarCountLens a b | a -> b where
  starCount :: Lens a b
instance StarCountLens SegmentDetailed Integer where
  starCount segmentDetailed =
    ( segmentDetailedStarCount segmentDetailed
    , \ segmentDetailedStarCount' -> segmentDetailed { segmentDetailedStarCount = segmentDetailedStarCount' }
    )

class StarredLens a b | a -> b where
  starred :: Lens a b
instance StarredLens SegmentDetailed Bool where
  starred segmentDetailed =
    ( segmentDetailedStarred segmentDetailed
    , \ segmentDetailedStarred' -> segmentDetailed { segmentDetailedStarred = segmentDetailedStarred' }
    )
instance StarredLens SegmentSummary Bool where
  starred segmentSummary =
    ( segmentSummaryStarred segmentSummary
    , \ segmentSummaryStarred' -> segmentSummary { segmentSummaryStarred = segmentSummaryStarred' }
    )
instance StarredLens SegmentExplorerEntry Bool where
  starred segmentExplorerEntry =
    ( segmentExplorerEntryStarred segmentExplorerEntry
    , \ segmentExplorerEntryStarred' -> segmentExplorerEntry { segmentExplorerEntryStarred = segmentExplorerEntryStarred' }
    )

class StartDateLens a b | a -> b where
  startDate :: Lens a b
instance StartDateLens ActivityDetailed UTCTime where
  startDate activityDetailed =
    ( activityDetailedStartDate activityDetailed
    , \ activityDetailedStartDate' -> activityDetailed { activityDetailedStartDate = activityDetailedStartDate' }
    )
instance StartDateLens ActivitySummary UTCTime where
  startDate activitySummary =
    ( activitySummaryStartDate activitySummary
    , \ activitySummaryStartDate' -> activitySummary { activitySummaryStartDate = activitySummaryStartDate' }
    )
instance StartDateLens ActivityLapSummary UTCTime where
  startDate activityLapSummary =
    ( activityLapSummaryStartDate activityLapSummary
    , \ activityLapSummaryStartDate' -> activityLapSummary { activityLapSummaryStartDate = activityLapSummaryStartDate' }
    )
instance StartDateLens EffortDetailed UTCTime where
  startDate effortDetailed =
    ( effortDetailedStartDate effortDetailed
    , \ effortDetailedStartDate' -> effortDetailed { effortDetailedStartDate = effortDetailedStartDate' }
    )
instance StartDateLens SegmentLeaderboardEntry UTCTime where
  startDate segmentLeaderboardEntry =
    ( segmentLeaderboardEntryStartDate segmentLeaderboardEntry
    , \ segmentLeaderboardEntryStartDate' -> segmentLeaderboardEntry { segmentLeaderboardEntryStartDate = segmentLeaderboardEntryStartDate' }
    )

class StartDateLocalLens a b | a -> b where
  startDateLocal :: Lens a b
instance StartDateLocalLens ActivityDetailed UTCTime where
  startDateLocal activityDetailed =
    ( activityDetailedStartDateLocal activityDetailed
    , \ activityDetailedStartDateLocal' -> activityDetailed { activityDetailedStartDateLocal = activityDetailedStartDateLocal' }
    )
instance StartDateLocalLens ActivitySummary UTCTime where
  startDateLocal activitySummary =
    ( activitySummaryStartDateLocal activitySummary
    , \ activitySummaryStartDateLocal' -> activitySummary { activitySummaryStartDateLocal = activitySummaryStartDateLocal' }
    )
instance StartDateLocalLens ActivityLapSummary UTCTime where
  startDateLocal activityLapSummary =
    ( activityLapSummaryStartDateLocal activityLapSummary
    , \ activityLapSummaryStartDateLocal' -> activityLapSummary { activityLapSummaryStartDateLocal = activityLapSummaryStartDateLocal' }
    )
instance StartDateLocalLens EffortDetailed UTCTime where
  startDateLocal effortDetailed =
    ( effortDetailedStartDateLocal effortDetailed
    , \ effortDetailedStartDateLocal' -> effortDetailed { effortDetailedStartDateLocal = effortDetailedStartDateLocal' }
    )
instance StartDateLocalLens SegmentLeaderboardEntry UTCTime where
  startDateLocal segmentLeaderboardEntry =
    ( segmentLeaderboardEntryStartDateLocal segmentLeaderboardEntry
    , \ segmentLeaderboardEntryStartDateLocal' -> segmentLeaderboardEntry { segmentLeaderboardEntryStartDateLocal = segmentLeaderboardEntryStartDateLocal' }
    )

class StartIndexLens a b | a -> b where
  startIndex :: Lens a b
instance StartIndexLens ActivityLapSummary Integer where
  startIndex activityLapSummary =
    ( activityLapSummaryStartIndex activityLapSummary
    , \ activityLapSummaryStartIndex' -> activityLapSummary { activityLapSummaryStartIndex = activityLapSummaryStartIndex' }
    )
instance StartIndexLens EffortDetailed Integer where
  startIndex effortDetailed =
    ( effortDetailedStartIndex effortDetailed
    , \ effortDetailedStartIndex' -> effortDetailed { effortDetailedStartIndex = effortDetailedStartIndex' }
    )

class StartLatitudeLens a b | a -> b where
  startLatitude :: Lens a b
instance StartLatitudeLens ActivityDetailed Double where
  startLatitude activityDetailed =
    ( activityDetailedStartLatitude activityDetailed
    , \ activityDetailedStartLatitude' -> activityDetailed { activityDetailedStartLatitude = activityDetailedStartLatitude' }
    )
instance StartLatitudeLens ActivitySummary Double where
  startLatitude activitySummary =
    ( activitySummaryStartLatitude activitySummary
    , \ activitySummaryStartLatitude' -> activitySummary { activitySummaryStartLatitude = activitySummaryStartLatitude' }
    )
instance StartLatitudeLens SegmentDetailed Double where
  startLatitude segmentDetailed =
    ( segmentDetailedStartLatitude segmentDetailed
    , \ segmentDetailedStartLatitude' -> segmentDetailed { segmentDetailedStartLatitude = segmentDetailedStartLatitude' }
    )
instance StartLatitudeLens SegmentSummary Double where
  startLatitude segmentSummary =
    ( segmentSummaryStartLatitude segmentSummary
    , \ segmentSummaryStartLatitude' -> segmentSummary { segmentSummaryStartLatitude = segmentSummaryStartLatitude' }
    )

class StartLatlngLens a b | a -> b where
  startLatlng :: Lens a b
instance StartLatlngLens ActivityDetailed (Maybe (Double, Double)) where
  startLatlng activityDetailed =
    ( activityDetailedStartLatlng activityDetailed
    , \ activityDetailedStartLatlng' -> activityDetailed { activityDetailedStartLatlng = activityDetailedStartLatlng' }
    )
instance StartLatlngLens ActivitySummary (Maybe (Double, Double)) where
  startLatlng activitySummary =
    ( activitySummaryStartLatlng activitySummary
    , \ activitySummaryStartLatlng' -> activitySummary { activitySummaryStartLatlng = activitySummaryStartLatlng' }
    )
instance StartLatlngLens SegmentDetailed ((Double, Double)) where
  startLatlng segmentDetailed =
    ( segmentDetailedStartLatlng segmentDetailed
    , \ segmentDetailedStartLatlng' -> segmentDetailed { segmentDetailedStartLatlng = segmentDetailedStartLatlng' }
    )
instance StartLatlngLens SegmentSummary ((Double, Double)) where
  startLatlng segmentSummary =
    ( segmentSummaryStartLatlng segmentSummary
    , \ segmentSummaryStartLatlng' -> segmentSummary { segmentSummaryStartLatlng = segmentSummaryStartLatlng' }
    )
instance StartLatlngLens SegmentExplorerEntry ((Double, Double)) where
  startLatlng segmentExplorerEntry =
    ( segmentExplorerEntryStartLatlng segmentExplorerEntry
    , \ segmentExplorerEntryStartLatlng' -> segmentExplorerEntry { segmentExplorerEntryStartLatlng = segmentExplorerEntryStartLatlng' }
    )

class StartLongitudeLens a b | a -> b where
  startLongitude :: Lens a b
instance StartLongitudeLens ActivityDetailed Double where
  startLongitude activityDetailed =
    ( activityDetailedStartLongitude activityDetailed
    , \ activityDetailedStartLongitude' -> activityDetailed { activityDetailedStartLongitude = activityDetailedStartLongitude' }
    )
instance StartLongitudeLens ActivitySummary Double where
  startLongitude activitySummary =
    ( activitySummaryStartLongitude activitySummary
    , \ activitySummaryStartLongitude' -> activitySummary { activitySummaryStartLongitude = activitySummaryStartLongitude' }
    )
instance StartLongitudeLens SegmentDetailed Double where
  startLongitude segmentDetailed =
    ( segmentDetailedStartLongitude segmentDetailed
    , \ segmentDetailedStartLongitude' -> segmentDetailed { segmentDetailedStartLongitude = segmentDetailedStartLongitude' }
    )
instance StartLongitudeLens SegmentSummary Double where
  startLongitude segmentSummary =
    ( segmentSummaryStartLongitude segmentSummary
    , \ segmentSummaryStartLongitude' -> segmentSummary { segmentSummaryStartLongitude = segmentSummaryStartLongitude' }
    )

class StateLens a b | a -> b where
  state :: Lens a b
instance StateLens AthleteDetailed Text where
  state athleteDetailed =
    ( athleteDetailedState athleteDetailed
    , \ athleteDetailedState' -> athleteDetailed { athleteDetailedState = athleteDetailedState' }
    )
instance StateLens AthleteSummary Text where
  state athleteSummary =
    ( athleteSummaryState athleteSummary
    , \ athleteSummaryState' -> athleteSummary { athleteSummaryState = athleteSummaryState' }
    )
instance StateLens ClubDetailed Text where
  state clubDetailed =
    ( clubDetailedState clubDetailed
    , \ clubDetailedState' -> clubDetailed { clubDetailedState = clubDetailedState' }
    )
instance StateLens SegmentDetailed Text where
  state segmentDetailed =
    ( segmentDetailedState segmentDetailed
    , \ segmentDetailedState' -> segmentDetailed { segmentDetailedState = segmentDetailedState' }
    )
instance StateLens SegmentSummary Text where
  state segmentSummary =
    ( segmentSummaryState segmentSummary
    , \ segmentSummaryState' -> segmentSummary { segmentSummaryState = segmentSummaryState' }
    )

class StatusLens a b | a -> b where
  status :: Lens a b
instance StatusLens UploadStatus Text where
  status uploadStatus =
    ( uploadStatusStatus uploadStatus
    , \ uploadStatusStatus' -> uploadStatus { uploadStatusStatus = uploadStatusStatus' }
    )

class SummaryPolylineLens a b | a -> b where
  summaryPolyline :: Lens a b
instance SummaryPolylineLens PolylineDetailed (Maybe [(Double, Double)]) where
  summaryPolyline polylineDetailed =
    ( polylineDetailedSummaryPolyline polylineDetailed
    , \ polylineDetailedSummaryPolyline' -> polylineDetailed { polylineDetailedSummaryPolyline = polylineDetailedSummaryPolyline' }
    )
instance SummaryPolylineLens PolylineSummary (Maybe [(Double, Double)]) where
  summaryPolyline polylineSummary =
    ( polylineSummarySummaryPolyline polylineSummary
    , \ polylineSummarySummaryPolyline' -> polylineSummary { polylineSummarySummaryPolyline = polylineSummarySummaryPolyline' }
    )

class TextLens a b | a -> b where
  text :: Lens a b
instance TextLens CommentSummary Text where
  text commentSummary =
    ( commentSummaryText commentSummary
    , \ commentSummaryText' -> commentSummary { commentSummaryText = commentSummaryText' }
    )

class TimeLens a b | a -> b where
  time :: Lens a b
instance TimeLens ActivityZoneDistributionBucket Integer where
  time activityZoneDistributionBucket =
    ( activityZoneDistributionBucketTime activityZoneDistributionBucket
    , \ activityZoneDistributionBucketTime' -> activityZoneDistributionBucket { activityZoneDistributionBucketTime = activityZoneDistributionBucketTime' }
    )

class TimezoneLens a b | a -> b where
  timezone :: Lens a b
instance TimezoneLens ActivityDetailed Text where
  timezone activityDetailed =
    ( activityDetailedTimezone activityDetailed
    , \ activityDetailedTimezone' -> activityDetailed { activityDetailedTimezone = activityDetailedTimezone' }
    )
instance TimezoneLens ActivitySummary Text where
  timezone activitySummary =
    ( activitySummaryTimezone activitySummary
    , \ activitySummaryTimezone' -> activitySummary { activitySummaryTimezone = activitySummaryTimezone' }
    )

class TotalElevationGainLens a b | a -> b where
  totalElevationGain :: Lens a b
instance TotalElevationGainLens ActivityDetailed Double where
  totalElevationGain activityDetailed =
    ( activityDetailedTotalElevationGain activityDetailed
    , \ activityDetailedTotalElevationGain' -> activityDetailed { activityDetailedTotalElevationGain = activityDetailedTotalElevationGain' }
    )
instance TotalElevationGainLens ActivitySummary Double where
  totalElevationGain activitySummary =
    ( activitySummaryTotalElevationGain activitySummary
    , \ activitySummaryTotalElevationGain' -> activitySummary { activitySummaryTotalElevationGain = activitySummaryTotalElevationGain' }
    )
instance TotalElevationGainLens ActivityLapSummary Double where
  totalElevationGain activityLapSummary =
    ( activityLapSummaryTotalElevationGain activityLapSummary
    , \ activityLapSummaryTotalElevationGain' -> activityLapSummary { activityLapSummaryTotalElevationGain = activityLapSummaryTotalElevationGain' }
    )
instance TotalElevationGainLens SegmentDetailed Double where
  totalElevationGain segmentDetailed =
    ( segmentDetailedTotalElevationGain segmentDetailed
    , \ segmentDetailedTotalElevationGain' -> segmentDetailed { segmentDetailedTotalElevationGain = segmentDetailedTotalElevationGain' }
    )

class TrainerLens a b | a -> b where
  trainer :: Lens a b
instance TrainerLens ActivityDetailed Bool where
  trainer activityDetailed =
    ( activityDetailedTrainer activityDetailed
    , \ activityDetailedTrainer' -> activityDetailed { activityDetailedTrainer = activityDetailedTrainer' }
    )
instance TrainerLens ActivitySummary Bool where
  trainer activitySummary =
    ( activitySummaryTrainer activitySummary
    , \ activitySummaryTrainer' -> activitySummary { activitySummaryTrainer = activitySummaryTrainer' }
    )

class TruncatedLens a b | a -> b where
  truncated :: Lens a b
instance TruncatedLens ActivityDetailed Integer where
  truncated activityDetailed =
    ( activityDetailedTruncated activityDetailed
    , \ activityDetailedTruncated' -> activityDetailed { activityDetailedTruncated = activityDetailedTruncated' }
    )

class TypeLens a b | a -> b where
  type' :: Lens a b
instance TypeLens ActivityDetailed Text where
  type' activityDetailed =
    ( activityDetailedType activityDetailed
    , \ activityDetailedType' -> activityDetailed { activityDetailedType = activityDetailedType' }
    )
instance TypeLens ActivitySummary Text where
  type' activitySummary =
    ( activitySummaryType activitySummary
    , \ activitySummaryType' -> activitySummary { activitySummaryType = activitySummaryType' }
    )
instance TypeLens ActivityZoneDetailed Text where
  type' activityZoneDetailed =
    ( activityZoneDetailedType activityZoneDetailed
    , \ activityZoneDetailedType' -> activityZoneDetailed { activityZoneDetailedType = activityZoneDetailedType' }
    )
instance TypeLens PhotoSummary Text where
  type' photoSummary =
    ( photoSummaryType photoSummary
    , \ photoSummaryType' -> photoSummary { photoSummaryType = photoSummaryType' }
    )
instance TypeLens StreamDetailed Text where
  type' streamDetailed =
    ( streamDetailedType streamDetailed
    , \ streamDetailedType' -> streamDetailed { streamDetailedType = streamDetailedType' }
    )

class UidLens a b | a -> b where
  uid :: Lens a b
instance UidLens PhotoSummary Text where
  uid photoSummary =
    ( photoSummaryUid photoSummary
    , \ photoSummaryUid' -> photoSummary { photoSummaryUid = photoSummaryUid' }
    )

class UpdatedAtLens a b | a -> b where
  updatedAt :: Lens a b
instance UpdatedAtLens AthleteDetailed UTCTime where
  updatedAt athleteDetailed =
    ( athleteDetailedUpdatedAt athleteDetailed
    , \ athleteDetailedUpdatedAt' -> athleteDetailed { athleteDetailedUpdatedAt = athleteDetailedUpdatedAt' }
    )
instance UpdatedAtLens AthleteSummary UTCTime where
  updatedAt athleteSummary =
    ( athleteSummaryUpdatedAt athleteSummary
    , \ athleteSummaryUpdatedAt' -> athleteSummary { athleteSummaryUpdatedAt = athleteSummaryUpdatedAt' }
    )
instance UpdatedAtLens SegmentDetailed UTCTime where
  updatedAt segmentDetailed =
    ( segmentDetailedUpdatedAt segmentDetailed
    , \ segmentDetailedUpdatedAt' -> segmentDetailed { segmentDetailedUpdatedAt = segmentDetailedUpdatedAt' }
    )

class UploadIdLens a b | a -> b where
  uploadId :: Lens a b
instance UploadIdLens ActivityDetailed (Maybe Integer) where
  uploadId activityDetailed =
    ( activityDetailedUploadId activityDetailed
    , \ activityDetailedUploadId' -> activityDetailed { activityDetailedUploadId = activityDetailedUploadId' }
    )
instance UploadIdLens ActivitySummary (Maybe Integer) where
  uploadId activitySummary =
    ( activitySummaryUploadId activitySummary
    , \ activitySummaryUploadId' -> activitySummary { activitySummaryUploadId = activitySummaryUploadId' }
    )

class UploadedAtLens a b | a -> b where
  uploadedAt :: Lens a b
instance UploadedAtLens PhotoSummary UTCTime where
  uploadedAt photoSummary =
    ( photoSummaryUploadedAt photoSummary
    , \ photoSummaryUploadedAt' -> photoSummary { photoSummaryUploadedAt = photoSummaryUploadedAt' }
    )
