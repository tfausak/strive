{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

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

-- * Classes

class AccessTokenLens a b | a -> b where
  accessToken' :: Lens a b
class AchievementCountLens a b | a -> b where
  achievementCount :: Lens a b
class ActivityIdLens a b | a -> b where
  activityId :: Lens a b
class ActivityTypeLens a b | a -> b where
  activityType :: Lens a b
class AthleteCountLens a b | a -> b where
  athleteCount :: Lens a b
class AthleteGenderLens a b | a -> b where
  athleteGender :: Lens a b
class AthleteIdLens a b | a -> b where
  athleteId :: Lens a b
class AthleteLens a b | a -> b where
  athlete :: Lens a b
class AthleteNameLens a b | a -> b where
  athleteName :: Lens a b
class AthleteProfileLens a b | a -> b where
  athleteProfile :: Lens a b
class AverageCadenceLens a b | a -> b where
  averageCadence :: Lens a b
class AverageGradeLens a b | a -> b where
  averageGrade :: Lens a b
class AverageHeartrateLens a b | a -> b where
  averageHeartrate :: Lens a b
class AverageHrLens a b | a -> b where
  averageHr :: Lens a b
class AverageSpeedLens a b | a -> b where
  averageSpeed :: Lens a b
class AverageWattsLens a b | a -> b where
  averageWatts :: Lens a b
class AvgGradeLens a b | a -> b where
  avgGrade :: Lens a b
class BikesLens a b | a -> b where
  bikes :: Lens a b
class BrandNameLens a b | a -> b where
  brandName :: Lens a b
class CaloriesLens a b | a -> b where
  calories :: Lens a b
class CaptionLens a b | a -> b where
  caption :: Lens a b
class CityLens a b | a -> b where
  city :: Lens a b
class ClimbCategoryDescLens a b | a -> b where
  climbCategoryDesc :: Lens a b
class ClimbCategoryLens a b | a -> b where
  climbCategory :: Lens a b
class ClubTypeLens a b | a -> b where
  clubType :: Lens a b
class ClubsLens a b | a -> b where
  clubs :: Lens a b
class CommentCountLens a b | a -> b where
  commentCount :: Lens a b
class CommuteLens a b | a -> b where
  commute :: Lens a b
class CountryLens a b | a -> b where
  country :: Lens a b
class CreatedAtLens a b | a -> b where
  createdAt :: Lens a b
class DataLens a b | a -> b where
  data' :: Lens a b
class DatePreferenceLens a b | a -> b where
  datePreference :: Lens a b
class DescriptionLens a b | a -> b where
  description :: Lens a b
class DistanceLens a b | a -> b where
  distance :: Lens a b
class DistributionBucketsLens a b | a -> b where
  distributionBuckets :: Lens a b
class EffortCountLens a b | a -> b where
  effortCount :: Lens a b
class EffortIdLens a b | a -> b where
  effortId :: Lens a b
class ElapsedTimeLens a b | a -> b where
  elapsedTime :: Lens a b
class ElevDifferenceLens a b | a -> b where
  elevDifference :: Lens a b
class ElevationHighLens a b | a -> b where
  elevationHigh :: Lens a b
class ElevationLowLens a b | a -> b where
  elevationLow :: Lens a b
class EmailLens a b | a -> b where
  email :: Lens a b
class EndIndexLens a b | a -> b where
  endIndex :: Lens a b
class EndLatitudeLens a b | a -> b where
  endLatitude :: Lens a b
class EndLatlngLens a b | a -> b where
  endLatlng :: Lens a b
class EndLongitudeLens a b | a -> b where
  endLongitude :: Lens a b
class EntriesLens a b | a -> b where
  entries :: Lens a b
class ErrorLens a b | a -> b where
  error :: Lens a b
class ExternalIdLens a b | a -> b where
  externalId :: Lens a b
class FirstnameLens a b | a -> b where
  firstname :: Lens a b
class FlaggedLens a b | a -> b where
  flagged :: Lens a b
class FollowerCountLens a b | a -> b where
  followerCount :: Lens a b
class FollowerLens a b | a -> b where
  follower :: Lens a b
class FrameTypeLens a b | a -> b where
  frameType :: Lens a b
class FriendCountLens a b | a -> b where
  friendCount :: Lens a b
class FriendLens a b | a -> b where
  friend :: Lens a b
class FtpLens a b | a -> b where
  ftp :: Lens a b
class GearIdLens a b | a -> b where
  gearId :: Lens a b
class GearLens a b | a -> b where
  gear :: Lens a b
class HasKudoedLens a b | a -> b where
  hasKudoed :: Lens a b
class HazardousLens a b | a -> b where
  hazardous :: Lens a b
class HiddenLens a b | a -> b where
  hidden :: Lens a b
class IdLens a b | a -> b where
  id :: Lens a b
class InstagramPrimaryPhotoLens a b | a -> b where
  instagramPrimaryPhoto :: Lens a b
class KilojoulesLens a b | a -> b where
  kilojoules :: Lens a b
class KomRankLens a b | a -> b where
  komRank :: Lens a b
class KudosCountLens a b | a -> b where
  kudosCount :: Lens a b
class LapIndexLens a b | a -> b where
  lapIndex :: Lens a b
class LastnameLens a b | a -> b where
  lastname :: Lens a b
class LocationCityLens a b | a -> b where
  locationCity :: Lens a b
class LocationCountryLens a b | a -> b where
  locationCountry :: Lens a b
class LocationLens a b | a -> b where
  location :: Lens a b
class LocationStateLens a b | a -> b where
  locationState :: Lens a b
class ManualLens a b | a -> b where
  manual :: Lens a b
class MapLens a b | a -> b where
  map :: Lens a b
class MaxLens a b | a -> b where
  max :: Lens a b
class MaxSpeedLens a b | a -> b where
  maxSpeed :: Lens a b
class Max_heartrateLens a b | a -> b where
  max_heartrate :: Lens a b
class MaximumGradeLens a b | a -> b where
  maximumGrade :: Lens a b
class MeasurementPreferenceLens a b | a -> b where
  measurementPreference :: Lens a b
class MemberCountLens a b | a -> b where
  memberCount :: Lens a b
class MinLens a b | a -> b where
  min :: Lens a b
class ModelNameLens a b | a -> b where
  modelName :: Lens a b
class MovingTimeLens a b | a -> b where
  movingTime :: Lens a b
class MutualFriendCountLens a b | a -> b where
  mutualFriendCount :: Lens a b
class NameLens a b | a -> b where
  name :: Lens a b
class OriginalSizeLens a b | a -> b where
  originalSize :: Lens a b
class PhotoCountLens a b | a -> b where
  photoCount :: Lens a b
class PointsLens a b | a -> b where
  points :: Lens a b
class PolylineLens a b | a -> b where
  polyline :: Lens a b
class PrRankLens a b | a -> b where
  prRank :: Lens a b
class PremiumLens a b | a -> b where
  premium :: Lens a b
class PrimaryLens a b | a -> b where
  primary :: Lens a b
class PrivateLens a b | a -> b where
  private :: Lens a b
class ProfileLens a b | a -> b where
  profile :: Lens a b
class ProfileMediumLens a b | a -> b where
  profileMedium :: Lens a b
class RankLens a b | a -> b where
  rank :: Lens a b
class RefLens a b | a -> b where
  ref :: Lens a b
class ResolutionLens a b | a -> b where
  resolution :: Lens a b
class ResourceStateLens a b | a -> b where
  resourceState :: Lens a b
class SegmentEffortsLens a b | a -> b where
  segmentEfforts :: Lens a b
class SegmentLens a b | a -> b where
  segment :: Lens a b
class SensorBasedLens a b | a -> b where
  sensorBased :: Lens a b
class SeriesTypeLens a b | a -> b where
  seriesType :: Lens a b
class SexLens a b | a -> b where
  sex :: Lens a b
class ShoesLens a b | a -> b where
  shoes :: Lens a b
class SportTypeLens a b | a -> b where
  sportType :: Lens a b
class StarCountLens a b | a -> b where
  starCount :: Lens a b
class StarredLens a b | a -> b where
  starred :: Lens a b
class StartDateLens a b | a -> b where
  startDate :: Lens a b
class StartDateLocalLens a b | a -> b where
  startDateLocal :: Lens a b
class StartIndexLens a b | a -> b where
  startIndex :: Lens a b
class StartLatitudeLens a b | a -> b where
  startLatitude :: Lens a b
class StartLatlngLens a b | a -> b where
  startLatlng :: Lens a b
class StartLongitudeLens a b | a -> b where
  startLongitude :: Lens a b
class StateLens a b | a -> b where
  state :: Lens a b
class StatusLens a b | a -> b where
  status :: Lens a b
class SummaryPolylineLens a b | a -> b where
  summaryPolyline :: Lens a b
class TextLens a b | a -> b where
  text :: Lens a b
class TimeLens a b | a -> b where
  time :: Lens a b
class TimezoneLens a b | a -> b where
  timezone :: Lens a b
class TotalElevationGainLens a b | a -> b where
  totalElevationGain :: Lens a b
class TrainerLens a b | a -> b where
  trainer :: Lens a b
class TruncatedLens a b | a -> b where
  truncated :: Lens a b
class TypeLens a b | a -> b where
  type' :: Lens a b
class UidLens a b | a -> b where
  uid :: Lens a b
class UpdatedAtLens a b | a -> b where
  updatedAt :: Lens a b
class UploadIdLens a b | a -> b where
  uploadId :: Lens a b
class UploadedAtLens a b | a -> b where
  uploadedAt :: Lens a b

-- * Instances


-- ** ActivityDetailed

instance AchievementCountLens ActivityDetailed (Integer) where
  achievementCount activityDetailed =
    ( activityDetailedAchievementCount activityDetailed
    , \ achievementCount' -> activityDetailed { activityDetailedAchievementCount = achievementCount' }
    )
instance AthleteLens ActivityDetailed (AthleteMeta) where
  athlete activityDetailed =
    ( activityDetailedAthlete activityDetailed
    , \ athlete' -> activityDetailed { activityDetailedAthlete = athlete' }
    )
instance AthleteCountLens ActivityDetailed (Integer) where
  athleteCount activityDetailed =
    ( activityDetailedAthleteCount activityDetailed
    , \ athleteCount' -> activityDetailed { activityDetailedAthleteCount = athleteCount' }
    )
instance AverageSpeedLens ActivityDetailed (Double) where
  averageSpeed activityDetailed =
    ( activityDetailedAverageSpeed activityDetailed
    , \ averageSpeed' -> activityDetailed { activityDetailedAverageSpeed = averageSpeed' }
    )
instance AverageWattsLens ActivityDetailed (Maybe Double) where
  averageWatts activityDetailed =
    ( activityDetailedAverageWatts activityDetailed
    , \ averageWatts' -> activityDetailed { activityDetailedAverageWatts = averageWatts' }
    )
instance CaloriesLens ActivityDetailed (Double) where
  calories activityDetailed =
    ( activityDetailedCalories activityDetailed
    , \ calories' -> activityDetailed { activityDetailedCalories = calories' }
    )
instance CommentCountLens ActivityDetailed (Integer) where
  commentCount activityDetailed =
    ( activityDetailedCommentCount activityDetailed
    , \ commentCount' -> activityDetailed { activityDetailedCommentCount = commentCount' }
    )
instance CommuteLens ActivityDetailed (Bool) where
  commute activityDetailed =
    ( activityDetailedCommute activityDetailed
    , \ commute' -> activityDetailed { activityDetailedCommute = commute' }
    )
instance DescriptionLens ActivityDetailed (Text) where
  description activityDetailed =
    ( activityDetailedDescription activityDetailed
    , \ description' -> activityDetailed { activityDetailedDescription = description' }
    )
instance DistanceLens ActivityDetailed (Double) where
  distance activityDetailed =
    ( activityDetailedDistance activityDetailed
    , \ distance' -> activityDetailed { activityDetailedDistance = distance' }
    )
instance ElapsedTimeLens ActivityDetailed (Integer) where
  elapsedTime activityDetailed =
    ( activityDetailedElapsedTime activityDetailed
    , \ elapsedTime' -> activityDetailed { activityDetailedElapsedTime = elapsedTime' }
    )
instance EndLatlngLens ActivityDetailed (Maybe (Double, Double)) where
  endLatlng activityDetailed =
    ( activityDetailedEndLatlng activityDetailed
    , \ endLatlng' -> activityDetailed { activityDetailedEndLatlng = endLatlng' }
    )
instance ExternalIdLens ActivityDetailed (Maybe Text) where
  externalId activityDetailed =
    ( activityDetailedExternalId activityDetailed
    , \ externalId' -> activityDetailed { activityDetailedExternalId = externalId' }
    )
instance FlaggedLens ActivityDetailed (Bool) where
  flagged activityDetailed =
    ( activityDetailedFlagged activityDetailed
    , \ flagged' -> activityDetailed { activityDetailedFlagged = flagged' }
    )
instance GearLens ActivityDetailed (GearSummary) where
  gear activityDetailed =
    ( activityDetailedGear activityDetailed
    , \ gear' -> activityDetailed { activityDetailedGear = gear' }
    )
instance GearIdLens ActivityDetailed (Maybe Text) where
  gearId activityDetailed =
    ( activityDetailedGearId activityDetailed
    , \ gearId' -> activityDetailed { activityDetailedGearId = gearId' }
    )
instance HasKudoedLens ActivityDetailed (Bool) where
  hasKudoed activityDetailed =
    ( activityDetailedHasKudoed activityDetailed
    , \ hasKudoed' -> activityDetailed { activityDetailedHasKudoed = hasKudoed' }
    )
instance IdLens ActivityDetailed (Integer) where
  id activityDetailed =
    ( activityDetailedId activityDetailed
    , \ id' -> activityDetailed { activityDetailedId = id' }
    )
instance InstagramPrimaryPhotoLens ActivityDetailed (Text) where
  instagramPrimaryPhoto activityDetailed =
    ( activityDetailedInstagramPrimaryPhoto activityDetailed
    , \ instagramPrimaryPhoto' -> activityDetailed { activityDetailedInstagramPrimaryPhoto = instagramPrimaryPhoto' }
    )
instance KilojoulesLens ActivityDetailed (Maybe Double) where
  kilojoules activityDetailed =
    ( activityDetailedKilojoules activityDetailed
    , \ kilojoules' -> activityDetailed { activityDetailedKilojoules = kilojoules' }
    )
instance LocationCityLens ActivityDetailed (Maybe Text) where
  locationCity activityDetailed =
    ( activityDetailedLocationCity activityDetailed
    , \ locationCity' -> activityDetailed { activityDetailedLocationCity = locationCity' }
    )
instance LocationCountryLens ActivityDetailed (Text) where
  locationCountry activityDetailed =
    ( activityDetailedLocationCountry activityDetailed
    , \ locationCountry' -> activityDetailed { activityDetailedLocationCountry = locationCountry' }
    )
instance LocationStateLens ActivityDetailed (Maybe Text) where
  locationState activityDetailed =
    ( activityDetailedLocationState activityDetailed
    , \ locationState' -> activityDetailed { activityDetailedLocationState = locationState' }
    )
instance ManualLens ActivityDetailed (Bool) where
  manual activityDetailed =
    ( activityDetailedManual activityDetailed
    , \ manual' -> activityDetailed { activityDetailedManual = manual' }
    )
instance MapLens ActivityDetailed (PolylineDetailed) where
  map activityDetailed =
    ( activityDetailedMap activityDetailed
    , \ map' -> activityDetailed { activityDetailedMap = map' }
    )
instance MaxSpeedLens ActivityDetailed (Double) where
  maxSpeed activityDetailed =
    ( activityDetailedMaxSpeed activityDetailed
    , \ maxSpeed' -> activityDetailed { activityDetailedMaxSpeed = maxSpeed' }
    )
instance MovingTimeLens ActivityDetailed (Integer) where
  movingTime activityDetailed =
    ( activityDetailedMovingTime activityDetailed
    , \ movingTime' -> activityDetailed { activityDetailedMovingTime = movingTime' }
    )
instance NameLens ActivityDetailed (Text) where
  name activityDetailed =
    ( activityDetailedName activityDetailed
    , \ name' -> activityDetailed { activityDetailedName = name' }
    )
instance PhotoCountLens ActivityDetailed (Integer) where
  photoCount activityDetailed =
    ( activityDetailedPhotoCount activityDetailed
    , \ photoCount' -> activityDetailed { activityDetailedPhotoCount = photoCount' }
    )
instance PrivateLens ActivityDetailed (Bool) where
  private activityDetailed =
    ( activityDetailedPrivate activityDetailed
    , \ private' -> activityDetailed { activityDetailedPrivate = private' }
    )
instance ResourceStateLens ActivityDetailed (Integer) where
  resourceState activityDetailed =
    ( activityDetailedResourceState activityDetailed
    , \ resourceState' -> activityDetailed { activityDetailedResourceState = resourceState' }
    )
instance SegmentEffortsLens ActivityDetailed ([EffortDetailed]) where
  segmentEfforts activityDetailed =
    ( activityDetailedSegmentEfforts activityDetailed
    , \ segmentEfforts' -> activityDetailed { activityDetailedSegmentEfforts = segmentEfforts' }
    )
instance StartDateLens ActivityDetailed (UTCTime) where
  startDate activityDetailed =
    ( activityDetailedStartDate activityDetailed
    , \ startDate' -> activityDetailed { activityDetailedStartDate = startDate' }
    )
instance StartDateLocalLens ActivityDetailed (UTCTime) where
  startDateLocal activityDetailed =
    ( activityDetailedStartDateLocal activityDetailed
    , \ startDateLocal' -> activityDetailed { activityDetailedStartDateLocal = startDateLocal' }
    )
instance StartLatitudeLens ActivityDetailed (Double) where
  startLatitude activityDetailed =
    ( activityDetailedStartLatitude activityDetailed
    , \ startLatitude' -> activityDetailed { activityDetailedStartLatitude = startLatitude' }
    )
instance StartLatlngLens ActivityDetailed (Maybe (Double, Double)) where
  startLatlng activityDetailed =
    ( activityDetailedStartLatlng activityDetailed
    , \ startLatlng' -> activityDetailed { activityDetailedStartLatlng = startLatlng' }
    )
instance StartLongitudeLens ActivityDetailed (Double) where
  startLongitude activityDetailed =
    ( activityDetailedStartLongitude activityDetailed
    , \ startLongitude' -> activityDetailed { activityDetailedStartLongitude = startLongitude' }
    )
instance TimezoneLens ActivityDetailed (Text) where
  timezone activityDetailed =
    ( activityDetailedTimezone activityDetailed
    , \ timezone' -> activityDetailed { activityDetailedTimezone = timezone' }
    )
instance TotalElevationGainLens ActivityDetailed (Double) where
  totalElevationGain activityDetailed =
    ( activityDetailedTotalElevationGain activityDetailed
    , \ totalElevationGain' -> activityDetailed { activityDetailedTotalElevationGain = totalElevationGain' }
    )
instance TrainerLens ActivityDetailed (Bool) where
  trainer activityDetailed =
    ( activityDetailedTrainer activityDetailed
    , \ trainer' -> activityDetailed { activityDetailedTrainer = trainer' }
    )
instance TruncatedLens ActivityDetailed (Integer) where
  truncated activityDetailed =
    ( activityDetailedTruncated activityDetailed
    , \ truncated' -> activityDetailed { activityDetailedTruncated = truncated' }
    )
instance TypeLens ActivityDetailed (Text) where
  type' activityDetailed =
    ( activityDetailedType activityDetailed
    , \ type'' -> activityDetailed { activityDetailedType = type'' }
    )
instance UploadIdLens ActivityDetailed (Maybe Integer) where
  uploadId activityDetailed =
    ( activityDetailedUploadId activityDetailed
    , \ uploadId' -> activityDetailed { activityDetailedUploadId = uploadId' }
    )

-- ** ActivityLapSummary

instance ActivityIdLens ActivityLapSummary (Integer) where
  activityId activityLapSummary =
    ( activityLapSummaryActivityId activityLapSummary
    , \ activityId' -> activityLapSummary { activityLapSummaryActivityId = activityId' }
    )
instance AthleteIdLens ActivityLapSummary (Integer) where
  athleteId activityLapSummary =
    ( activityLapSummaryAthleteId activityLapSummary
    , \ athleteId' -> activityLapSummary { activityLapSummaryAthleteId = athleteId' }
    )
instance AverageSpeedLens ActivityLapSummary (Double) where
  averageSpeed activityLapSummary =
    ( activityLapSummaryAverageSpeed activityLapSummary
    , \ averageSpeed' -> activityLapSummary { activityLapSummaryAverageSpeed = averageSpeed' }
    )
instance AverageWattsLens ActivityLapSummary (Double) where
  averageWatts activityLapSummary =
    ( activityLapSummaryAverageWatts activityLapSummary
    , \ averageWatts' -> activityLapSummary { activityLapSummaryAverageWatts = averageWatts' }
    )
instance DistanceLens ActivityLapSummary (Double) where
  distance activityLapSummary =
    ( activityLapSummaryDistance activityLapSummary
    , \ distance' -> activityLapSummary { activityLapSummaryDistance = distance' }
    )
instance ElapsedTimeLens ActivityLapSummary (Integer) where
  elapsedTime activityLapSummary =
    ( activityLapSummaryElapsedTime activityLapSummary
    , \ elapsedTime' -> activityLapSummary { activityLapSummaryElapsedTime = elapsedTime' }
    )
instance EndIndexLens ActivityLapSummary (Integer) where
  endIndex activityLapSummary =
    ( activityLapSummaryEndIndex activityLapSummary
    , \ endIndex' -> activityLapSummary { activityLapSummaryEndIndex = endIndex' }
    )
instance IdLens ActivityLapSummary (Integer) where
  id activityLapSummary =
    ( activityLapSummaryId activityLapSummary
    , \ id' -> activityLapSummary { activityLapSummaryId = id' }
    )
instance LapIndexLens ActivityLapSummary (Integer) where
  lapIndex activityLapSummary =
    ( activityLapSummaryLapIndex activityLapSummary
    , \ lapIndex' -> activityLapSummary { activityLapSummaryLapIndex = lapIndex' }
    )
instance MaxSpeedLens ActivityLapSummary (Double) where
  maxSpeed activityLapSummary =
    ( activityLapSummaryMaxSpeed activityLapSummary
    , \ maxSpeed' -> activityLapSummary { activityLapSummaryMaxSpeed = maxSpeed' }
    )
instance MovingTimeLens ActivityLapSummary (Double) where
  movingTime activityLapSummary =
    ( activityLapSummaryMovingTime activityLapSummary
    , \ movingTime' -> activityLapSummary { activityLapSummaryMovingTime = movingTime' }
    )
instance NameLens ActivityLapSummary (Text) where
  name activityLapSummary =
    ( activityLapSummaryName activityLapSummary
    , \ name' -> activityLapSummary { activityLapSummaryName = name' }
    )
instance ResourceStateLens ActivityLapSummary (Integer) where
  resourceState activityLapSummary =
    ( activityLapSummaryResourceState activityLapSummary
    , \ resourceState' -> activityLapSummary { activityLapSummaryResourceState = resourceState' }
    )
instance StartDateLens ActivityLapSummary (UTCTime) where
  startDate activityLapSummary =
    ( activityLapSummaryStartDate activityLapSummary
    , \ startDate' -> activityLapSummary { activityLapSummaryStartDate = startDate' }
    )
instance StartDateLocalLens ActivityLapSummary (UTCTime) where
  startDateLocal activityLapSummary =
    ( activityLapSummaryStartDateLocal activityLapSummary
    , \ startDateLocal' -> activityLapSummary { activityLapSummaryStartDateLocal = startDateLocal' }
    )
instance StartIndexLens ActivityLapSummary (Integer) where
  startIndex activityLapSummary =
    ( activityLapSummaryStartIndex activityLapSummary
    , \ startIndex' -> activityLapSummary { activityLapSummaryStartIndex = startIndex' }
    )
instance TotalElevationGainLens ActivityLapSummary (Double) where
  totalElevationGain activityLapSummary =
    ( activityLapSummaryTotalElevationGain activityLapSummary
    , \ totalElevationGain' -> activityLapSummary { activityLapSummaryTotalElevationGain = totalElevationGain' }
    )

-- ** ActivitySummary

instance AchievementCountLens ActivitySummary (Integer) where
  achievementCount activitySummary =
    ( activitySummaryAchievementCount activitySummary
    , \ achievementCount' -> activitySummary { activitySummaryAchievementCount = achievementCount' }
    )
instance AthleteLens ActivitySummary (AthleteMeta) where
  athlete activitySummary =
    ( activitySummaryAthlete activitySummary
    , \ athlete' -> activitySummary { activitySummaryAthlete = athlete' }
    )
instance AthleteCountLens ActivitySummary (Integer) where
  athleteCount activitySummary =
    ( activitySummaryAthleteCount activitySummary
    , \ athleteCount' -> activitySummary { activitySummaryAthleteCount = athleteCount' }
    )
instance AverageSpeedLens ActivitySummary (Double) where
  averageSpeed activitySummary =
    ( activitySummaryAverageSpeed activitySummary
    , \ averageSpeed' -> activitySummary { activitySummaryAverageSpeed = averageSpeed' }
    )
instance AverageWattsLens ActivitySummary (Maybe Double) where
  averageWatts activitySummary =
    ( activitySummaryAverageWatts activitySummary
    , \ averageWatts' -> activitySummary { activitySummaryAverageWatts = averageWatts' }
    )
instance CommentCountLens ActivitySummary (Integer) where
  commentCount activitySummary =
    ( activitySummaryCommentCount activitySummary
    , \ commentCount' -> activitySummary { activitySummaryCommentCount = commentCount' }
    )
instance CommuteLens ActivitySummary (Bool) where
  commute activitySummary =
    ( activitySummaryCommute activitySummary
    , \ commute' -> activitySummary { activitySummaryCommute = commute' }
    )
instance DistanceLens ActivitySummary (Double) where
  distance activitySummary =
    ( activitySummaryDistance activitySummary
    , \ distance' -> activitySummary { activitySummaryDistance = distance' }
    )
instance ElapsedTimeLens ActivitySummary (Integer) where
  elapsedTime activitySummary =
    ( activitySummaryElapsedTime activitySummary
    , \ elapsedTime' -> activitySummary { activitySummaryElapsedTime = elapsedTime' }
    )
instance EndLatlngLens ActivitySummary (Maybe (Double, Double)) where
  endLatlng activitySummary =
    ( activitySummaryEndLatlng activitySummary
    , \ endLatlng' -> activitySummary { activitySummaryEndLatlng = endLatlng' }
    )
instance ExternalIdLens ActivitySummary (Maybe Text) where
  externalId activitySummary =
    ( activitySummaryExternalId activitySummary
    , \ externalId' -> activitySummary { activitySummaryExternalId = externalId' }
    )
instance FlaggedLens ActivitySummary (Bool) where
  flagged activitySummary =
    ( activitySummaryFlagged activitySummary
    , \ flagged' -> activitySummary { activitySummaryFlagged = flagged' }
    )
instance GearIdLens ActivitySummary (Maybe Text) where
  gearId activitySummary =
    ( activitySummaryGearId activitySummary
    , \ gearId' -> activitySummary { activitySummaryGearId = gearId' }
    )
instance HasKudoedLens ActivitySummary (Bool) where
  hasKudoed activitySummary =
    ( activitySummaryHasKudoed activitySummary
    , \ hasKudoed' -> activitySummary { activitySummaryHasKudoed = hasKudoed' }
    )
instance IdLens ActivitySummary (Integer) where
  id activitySummary =
    ( activitySummaryId activitySummary
    , \ id' -> activitySummary { activitySummaryId = id' }
    )
instance KilojoulesLens ActivitySummary (Maybe Double) where
  kilojoules activitySummary =
    ( activitySummaryKilojoules activitySummary
    , \ kilojoules' -> activitySummary { activitySummaryKilojoules = kilojoules' }
    )
instance KudosCountLens ActivitySummary (Integer) where
  kudosCount activitySummary =
    ( activitySummaryKudosCount activitySummary
    , \ kudosCount' -> activitySummary { activitySummaryKudosCount = kudosCount' }
    )
instance LocationCityLens ActivitySummary (Maybe Text) where
  locationCity activitySummary =
    ( activitySummaryLocationCity activitySummary
    , \ locationCity' -> activitySummary { activitySummaryLocationCity = locationCity' }
    )
instance LocationCountryLens ActivitySummary (Text) where
  locationCountry activitySummary =
    ( activitySummaryLocationCountry activitySummary
    , \ locationCountry' -> activitySummary { activitySummaryLocationCountry = locationCountry' }
    )
instance LocationStateLens ActivitySummary (Maybe Text) where
  locationState activitySummary =
    ( activitySummaryLocationState activitySummary
    , \ locationState' -> activitySummary { activitySummaryLocationState = locationState' }
    )
instance ManualLens ActivitySummary (Bool) where
  manual activitySummary =
    ( activitySummaryManual activitySummary
    , \ manual' -> activitySummary { activitySummaryManual = manual' }
    )
instance MapLens ActivitySummary (PolylineSummary) where
  map activitySummary =
    ( activitySummaryMap activitySummary
    , \ map' -> activitySummary { activitySummaryMap = map' }
    )
instance MaxSpeedLens ActivitySummary (Double) where
  maxSpeed activitySummary =
    ( activitySummaryMaxSpeed activitySummary
    , \ maxSpeed' -> activitySummary { activitySummaryMaxSpeed = maxSpeed' }
    )
instance MovingTimeLens ActivitySummary (Integer) where
  movingTime activitySummary =
    ( activitySummaryMovingTime activitySummary
    , \ movingTime' -> activitySummary { activitySummaryMovingTime = movingTime' }
    )
instance NameLens ActivitySummary (Text) where
  name activitySummary =
    ( activitySummaryName activitySummary
    , \ name' -> activitySummary { activitySummaryName = name' }
    )
instance PhotoCountLens ActivitySummary (Integer) where
  photoCount activitySummary =
    ( activitySummaryPhotoCount activitySummary
    , \ photoCount' -> activitySummary { activitySummaryPhotoCount = photoCount' }
    )
instance PrivateLens ActivitySummary (Bool) where
  private activitySummary =
    ( activitySummaryPrivate activitySummary
    , \ private' -> activitySummary { activitySummaryPrivate = private' }
    )
instance ResourceStateLens ActivitySummary (Integer) where
  resourceState activitySummary =
    ( activitySummaryResourceState activitySummary
    , \ resourceState' -> activitySummary { activitySummaryResourceState = resourceState' }
    )
instance StartDateLens ActivitySummary (UTCTime) where
  startDate activitySummary =
    ( activitySummaryStartDate activitySummary
    , \ startDate' -> activitySummary { activitySummaryStartDate = startDate' }
    )
instance StartDateLocalLens ActivitySummary (UTCTime) where
  startDateLocal activitySummary =
    ( activitySummaryStartDateLocal activitySummary
    , \ startDateLocal' -> activitySummary { activitySummaryStartDateLocal = startDateLocal' }
    )
instance StartLatitudeLens ActivitySummary (Double) where
  startLatitude activitySummary =
    ( activitySummaryStartLatitude activitySummary
    , \ startLatitude' -> activitySummary { activitySummaryStartLatitude = startLatitude' }
    )
instance StartLatlngLens ActivitySummary (Maybe (Double, Double)) where
  startLatlng activitySummary =
    ( activitySummaryStartLatlng activitySummary
    , \ startLatlng' -> activitySummary { activitySummaryStartLatlng = startLatlng' }
    )
instance StartLongitudeLens ActivitySummary (Double) where
  startLongitude activitySummary =
    ( activitySummaryStartLongitude activitySummary
    , \ startLongitude' -> activitySummary { activitySummaryStartLongitude = startLongitude' }
    )
instance TimezoneLens ActivitySummary (Text) where
  timezone activitySummary =
    ( activitySummaryTimezone activitySummary
    , \ timezone' -> activitySummary { activitySummaryTimezone = timezone' }
    )
instance TotalElevationGainLens ActivitySummary (Double) where
  totalElevationGain activitySummary =
    ( activitySummaryTotalElevationGain activitySummary
    , \ totalElevationGain' -> activitySummary { activitySummaryTotalElevationGain = totalElevationGain' }
    )
instance TrainerLens ActivitySummary (Bool) where
  trainer activitySummary =
    ( activitySummaryTrainer activitySummary
    , \ trainer' -> activitySummary { activitySummaryTrainer = trainer' }
    )
instance TypeLens ActivitySummary (Text) where
  type' activitySummary =
    ( activitySummaryType activitySummary
    , \ type'' -> activitySummary { activitySummaryType = type'' }
    )
instance UploadIdLens ActivitySummary (Maybe Integer) where
  uploadId activitySummary =
    ( activitySummaryUploadId activitySummary
    , \ uploadId' -> activitySummary { activitySummaryUploadId = uploadId' }
    )

-- ** ActivityZoneDetailed

instance DistributionBucketsLens ActivityZoneDetailed ([ActivityZoneDistributionBucket]) where
  distributionBuckets activityZoneDetailed =
    ( activityZoneDetailedDistributionBuckets activityZoneDetailed
    , \ distributionBuckets' -> activityZoneDetailed { activityZoneDetailedDistributionBuckets = distributionBuckets' }
    )
instance ResourceStateLens ActivityZoneDetailed (Integer) where
  resourceState activityZoneDetailed =
    ( activityZoneDetailedResourceState activityZoneDetailed
    , \ resourceState' -> activityZoneDetailed { activityZoneDetailedResourceState = resourceState' }
    )
instance SensorBasedLens ActivityZoneDetailed (Bool) where
  sensorBased activityZoneDetailed =
    ( activityZoneDetailedSensorBased activityZoneDetailed
    , \ sensorBased' -> activityZoneDetailed { activityZoneDetailedSensorBased = sensorBased' }
    )
instance TypeLens ActivityZoneDetailed (Text) where
  type' activityZoneDetailed =
    ( activityZoneDetailedType activityZoneDetailed
    , \ type'' -> activityZoneDetailed { activityZoneDetailedType = type'' }
    )

-- ** ActivityZoneDistributionBucket

instance MaxLens ActivityZoneDistributionBucket (Integer) where
  max activityZoneDistributionBucket =
    ( activityZoneDistributionBucketMax activityZoneDistributionBucket
    , \ max' -> activityZoneDistributionBucket { activityZoneDistributionBucketMax = max' }
    )
instance MinLens ActivityZoneDistributionBucket (Integer) where
  min activityZoneDistributionBucket =
    ( activityZoneDistributionBucketMin activityZoneDistributionBucket
    , \ min' -> activityZoneDistributionBucket { activityZoneDistributionBucketMin = min' }
    )
instance TimeLens ActivityZoneDistributionBucket (Integer) where
  time activityZoneDistributionBucket =
    ( activityZoneDistributionBucketTime activityZoneDistributionBucket
    , \ time' -> activityZoneDistributionBucket { activityZoneDistributionBucketTime = time' }
    )

-- ** AthleteDetailed

instance BikesLens AthleteDetailed ([GearSummary]) where
  bikes athleteDetailed =
    ( athleteDetailedBikes athleteDetailed
    , \ bikes' -> athleteDetailed { athleteDetailedBikes = bikes' }
    )
instance CityLens AthleteDetailed (Text) where
  city athleteDetailed =
    ( athleteDetailedCity athleteDetailed
    , \ city' -> athleteDetailed { athleteDetailedCity = city' }
    )
instance ClubsLens AthleteDetailed ([ClubSummary]) where
  clubs athleteDetailed =
    ( athleteDetailedClubs athleteDetailed
    , \ clubs' -> athleteDetailed { athleteDetailedClubs = clubs' }
    )
instance CountryLens AthleteDetailed (Text) where
  country athleteDetailed =
    ( athleteDetailedCountry athleteDetailed
    , \ country' -> athleteDetailed { athleteDetailedCountry = country' }
    )
instance CreatedAtLens AthleteDetailed (UTCTime) where
  createdAt athleteDetailed =
    ( athleteDetailedCreatedAt athleteDetailed
    , \ createdAt' -> athleteDetailed { athleteDetailedCreatedAt = createdAt' }
    )
instance DatePreferenceLens AthleteDetailed (Text) where
  datePreference athleteDetailed =
    ( athleteDetailedDatePreference athleteDetailed
    , \ datePreference' -> athleteDetailed { athleteDetailedDatePreference = datePreference' }
    )
instance EmailLens AthleteDetailed (Text) where
  email athleteDetailed =
    ( athleteDetailedEmail athleteDetailed
    , \ email' -> athleteDetailed { athleteDetailedEmail = email' }
    )
instance FirstnameLens AthleteDetailed (Text) where
  firstname athleteDetailed =
    ( athleteDetailedFirstname athleteDetailed
    , \ firstname' -> athleteDetailed { athleteDetailedFirstname = firstname' }
    )
instance FollowerLens AthleteDetailed (Maybe Text) where
  follower athleteDetailed =
    ( athleteDetailedFollower athleteDetailed
    , \ follower' -> athleteDetailed { athleteDetailedFollower = follower' }
    )
instance FollowerCountLens AthleteDetailed (Integer) where
  followerCount athleteDetailed =
    ( athleteDetailedFollowerCount athleteDetailed
    , \ followerCount' -> athleteDetailed { athleteDetailedFollowerCount = followerCount' }
    )
instance FriendLens AthleteDetailed (Maybe Text) where
  friend athleteDetailed =
    ( athleteDetailedFriend athleteDetailed
    , \ friend' -> athleteDetailed { athleteDetailedFriend = friend' }
    )
instance FriendCountLens AthleteDetailed (Integer) where
  friendCount athleteDetailed =
    ( athleteDetailedFriendCount athleteDetailed
    , \ friendCount' -> athleteDetailed { athleteDetailedFriendCount = friendCount' }
    )
instance FtpLens AthleteDetailed (Maybe Integer) where
  ftp athleteDetailed =
    ( athleteDetailedFtp athleteDetailed
    , \ ftp' -> athleteDetailed { athleteDetailedFtp = ftp' }
    )
instance IdLens AthleteDetailed (Integer) where
  id athleteDetailed =
    ( athleteDetailedId athleteDetailed
    , \ id' -> athleteDetailed { athleteDetailedId = id' }
    )
instance LastnameLens AthleteDetailed (Text) where
  lastname athleteDetailed =
    ( athleteDetailedLastname athleteDetailed
    , \ lastname' -> athleteDetailed { athleteDetailedLastname = lastname' }
    )
instance MeasurementPreferenceLens AthleteDetailed (Text) where
  measurementPreference athleteDetailed =
    ( athleteDetailedMeasurementPreference athleteDetailed
    , \ measurementPreference' -> athleteDetailed { athleteDetailedMeasurementPreference = measurementPreference' }
    )
instance MutualFriendCountLens AthleteDetailed (Integer) where
  mutualFriendCount athleteDetailed =
    ( athleteDetailedMutualFriendCount athleteDetailed
    , \ mutualFriendCount' -> athleteDetailed { athleteDetailedMutualFriendCount = mutualFriendCount' }
    )
instance PremiumLens AthleteDetailed (Bool) where
  premium athleteDetailed =
    ( athleteDetailedPremium athleteDetailed
    , \ premium' -> athleteDetailed { athleteDetailedPremium = premium' }
    )
instance ProfileLens AthleteDetailed (Text) where
  profile athleteDetailed =
    ( athleteDetailedProfile athleteDetailed
    , \ profile' -> athleteDetailed { athleteDetailedProfile = profile' }
    )
instance ProfileMediumLens AthleteDetailed (Text) where
  profileMedium athleteDetailed =
    ( athleteDetailedProfileMedium athleteDetailed
    , \ profileMedium' -> athleteDetailed { athleteDetailedProfileMedium = profileMedium' }
    )
instance ResourceStateLens AthleteDetailed (Integer) where
  resourceState athleteDetailed =
    ( athleteDetailedResourceState athleteDetailed
    , \ resourceState' -> athleteDetailed { athleteDetailedResourceState = resourceState' }
    )
instance SexLens AthleteDetailed (Maybe Char) where
  sex athleteDetailed =
    ( athleteDetailedSex athleteDetailed
    , \ sex' -> athleteDetailed { athleteDetailedSex = sex' }
    )
instance ShoesLens AthleteDetailed ([GearSummary]) where
  shoes athleteDetailed =
    ( athleteDetailedShoes athleteDetailed
    , \ shoes' -> athleteDetailed { athleteDetailedShoes = shoes' }
    )
instance StateLens AthleteDetailed (Text) where
  state athleteDetailed =
    ( athleteDetailedState athleteDetailed
    , \ state' -> athleteDetailed { athleteDetailedState = state' }
    )
instance UpdatedAtLens AthleteDetailed (UTCTime) where
  updatedAt athleteDetailed =
    ( athleteDetailedUpdatedAt athleteDetailed
    , \ updatedAt' -> athleteDetailed { athleteDetailedUpdatedAt = updatedAt' }
    )

-- ** AthleteMeta

instance IdLens AthleteMeta (Integer) where
  id athleteMeta =
    ( athleteMetaId athleteMeta
    , \ id' -> athleteMeta { athleteMetaId = id' }
    )
instance ResourceStateLens AthleteMeta (Integer) where
  resourceState athleteMeta =
    ( athleteMetaResourceState athleteMeta
    , \ resourceState' -> athleteMeta { athleteMetaResourceState = resourceState' }
    )

-- ** AthleteSummary

instance CityLens AthleteSummary (Maybe Text) where
  city athleteSummary =
    ( athleteSummaryCity athleteSummary
    , \ city' -> athleteSummary { athleteSummaryCity = city' }
    )
instance CountryLens AthleteSummary (Maybe Text) where
  country athleteSummary =
    ( athleteSummaryCountry athleteSummary
    , \ country' -> athleteSummary { athleteSummaryCountry = country' }
    )
instance CreatedAtLens AthleteSummary (UTCTime) where
  createdAt athleteSummary =
    ( athleteSummaryCreatedAt athleteSummary
    , \ createdAt' -> athleteSummary { athleteSummaryCreatedAt = createdAt' }
    )
instance FirstnameLens AthleteSummary (Text) where
  firstname athleteSummary =
    ( athleteSummaryFirstname athleteSummary
    , \ firstname' -> athleteSummary { athleteSummaryFirstname = firstname' }
    )
instance FollowerLens AthleteSummary (Maybe Text) where
  follower athleteSummary =
    ( athleteSummaryFollower athleteSummary
    , \ follower' -> athleteSummary { athleteSummaryFollower = follower' }
    )
instance FriendLens AthleteSummary (Maybe Text) where
  friend athleteSummary =
    ( athleteSummaryFriend athleteSummary
    , \ friend' -> athleteSummary { athleteSummaryFriend = friend' }
    )
instance IdLens AthleteSummary (Integer) where
  id athleteSummary =
    ( athleteSummaryId athleteSummary
    , \ id' -> athleteSummary { athleteSummaryId = id' }
    )
instance LastnameLens AthleteSummary (Text) where
  lastname athleteSummary =
    ( athleteSummaryLastname athleteSummary
    , \ lastname' -> athleteSummary { athleteSummaryLastname = lastname' }
    )
instance PremiumLens AthleteSummary (Bool) where
  premium athleteSummary =
    ( athleteSummaryPremium athleteSummary
    , \ premium' -> athleteSummary { athleteSummaryPremium = premium' }
    )
instance ProfileLens AthleteSummary (Text) where
  profile athleteSummary =
    ( athleteSummaryProfile athleteSummary
    , \ profile' -> athleteSummary { athleteSummaryProfile = profile' }
    )
instance ProfileMediumLens AthleteSummary (Text) where
  profileMedium athleteSummary =
    ( athleteSummaryProfileMedium athleteSummary
    , \ profileMedium' -> athleteSummary { athleteSummaryProfileMedium = profileMedium' }
    )
instance ResourceStateLens AthleteSummary (Integer) where
  resourceState athleteSummary =
    ( athleteSummaryResourceState athleteSummary
    , \ resourceState' -> athleteSummary { athleteSummaryResourceState = resourceState' }
    )
instance SexLens AthleteSummary (Maybe Char) where
  sex athleteSummary =
    ( athleteSummarySex athleteSummary
    , \ sex' -> athleteSummary { athleteSummarySex = sex' }
    )
instance StateLens AthleteSummary (Text) where
  state athleteSummary =
    ( athleteSummaryState athleteSummary
    , \ state' -> athleteSummary { athleteSummaryState = state' }
    )
instance UpdatedAtLens AthleteSummary (UTCTime) where
  updatedAt athleteSummary =
    ( athleteSummaryUpdatedAt athleteSummary
    , \ updatedAt' -> athleteSummary { athleteSummaryUpdatedAt = updatedAt' }
    )

-- ** ClubDetailed

instance CityLens ClubDetailed (Text) where
  city clubDetailed =
    ( clubDetailedCity clubDetailed
    , \ city' -> clubDetailed { clubDetailedCity = city' }
    )
instance ClubTypeLens ClubDetailed (Text) where
  clubType clubDetailed =
    ( clubDetailedClubType clubDetailed
    , \ clubType' -> clubDetailed { clubDetailedClubType = clubType' }
    )
instance CountryLens ClubDetailed (Text) where
  country clubDetailed =
    ( clubDetailedCountry clubDetailed
    , \ country' -> clubDetailed { clubDetailedCountry = country' }
    )
instance DescriptionLens ClubDetailed (Text) where
  description clubDetailed =
    ( clubDetailedDescription clubDetailed
    , \ description' -> clubDetailed { clubDetailedDescription = description' }
    )
instance IdLens ClubDetailed (Integer) where
  id clubDetailed =
    ( clubDetailedId clubDetailed
    , \ id' -> clubDetailed { clubDetailedId = id' }
    )
instance MemberCountLens ClubDetailed (Integer) where
  memberCount clubDetailed =
    ( clubDetailedMemberCount clubDetailed
    , \ memberCount' -> clubDetailed { clubDetailedMemberCount = memberCount' }
    )
instance NameLens ClubDetailed (Text) where
  name clubDetailed =
    ( clubDetailedName clubDetailed
    , \ name' -> clubDetailed { clubDetailedName = name' }
    )
instance PrivateLens ClubDetailed (Bool) where
  private clubDetailed =
    ( clubDetailedPrivate clubDetailed
    , \ private' -> clubDetailed { clubDetailedPrivate = private' }
    )
instance ProfileLens ClubDetailed (Text) where
  profile clubDetailed =
    ( clubDetailedProfile clubDetailed
    , \ profile' -> clubDetailed { clubDetailedProfile = profile' }
    )
instance ProfileMediumLens ClubDetailed (Text) where
  profileMedium clubDetailed =
    ( clubDetailedProfileMedium clubDetailed
    , \ profileMedium' -> clubDetailed { clubDetailedProfileMedium = profileMedium' }
    )
instance ResourceStateLens ClubDetailed (Integer) where
  resourceState clubDetailed =
    ( clubDetailedResourceState clubDetailed
    , \ resourceState' -> clubDetailed { clubDetailedResourceState = resourceState' }
    )
instance SportTypeLens ClubDetailed (Text) where
  sportType clubDetailed =
    ( clubDetailedSportType clubDetailed
    , \ sportType' -> clubDetailed { clubDetailedSportType = sportType' }
    )
instance StateLens ClubDetailed (Text) where
  state clubDetailed =
    ( clubDetailedState clubDetailed
    , \ state' -> clubDetailed { clubDetailedState = state' }
    )

-- ** ClubSummary

instance IdLens ClubSummary (Integer) where
  id clubSummary =
    ( clubSummaryId clubSummary
    , \ id' -> clubSummary { clubSummaryId = id' }
    )
instance NameLens ClubSummary (Text) where
  name clubSummary =
    ( clubSummaryName clubSummary
    , \ name' -> clubSummary { clubSummaryName = name' }
    )
instance ProfileLens ClubSummary (Text) where
  profile clubSummary =
    ( clubSummaryProfile clubSummary
    , \ profile' -> clubSummary { clubSummaryProfile = profile' }
    )
instance ProfileMediumLens ClubSummary (Text) where
  profileMedium clubSummary =
    ( clubSummaryProfileMedium clubSummary
    , \ profileMedium' -> clubSummary { clubSummaryProfileMedium = profileMedium' }
    )
instance ResourceStateLens ClubSummary (Integer) where
  resourceState clubSummary =
    ( clubSummaryResourceState clubSummary
    , \ resourceState' -> clubSummary { clubSummaryResourceState = resourceState' }
    )

-- ** CommentSummary

instance ActivityIdLens CommentSummary (Integer) where
  activityId commentSummary =
    ( commentSummaryActivityId commentSummary
    , \ activityId' -> commentSummary { commentSummaryActivityId = activityId' }
    )
instance AthleteLens CommentSummary (AthleteSummary) where
  athlete commentSummary =
    ( commentSummaryAthlete commentSummary
    , \ athlete' -> commentSummary { commentSummaryAthlete = athlete' }
    )
instance CreatedAtLens CommentSummary (UTCTime) where
  createdAt commentSummary =
    ( commentSummaryCreatedAt commentSummary
    , \ createdAt' -> commentSummary { commentSummaryCreatedAt = createdAt' }
    )
instance IdLens CommentSummary (Integer) where
  id commentSummary =
    ( commentSummaryId commentSummary
    , \ id' -> commentSummary { commentSummaryId = id' }
    )
instance ResourceStateLens CommentSummary (Integer) where
  resourceState commentSummary =
    ( commentSummaryResourceState commentSummary
    , \ resourceState' -> commentSummary { commentSummaryResourceState = resourceState' }
    )
instance TextLens CommentSummary (Text) where
  text commentSummary =
    ( commentSummaryText commentSummary
    , \ text' -> commentSummary { commentSummaryText = text' }
    )

-- ** DeauthorizationResponse

instance AccessTokenLens DeauthorizationResponse (Text) where
  accessToken' deauthorizationResponse =
    ( deauthorizationResponseAccessToken deauthorizationResponse
    , \ accessToken'' -> deauthorizationResponse { deauthorizationResponseAccessToken = accessToken'' }
    )

-- ** EffortDetailed

instance ActivityIdLens EffortDetailed (Integer) where
  activityId effortDetailed =
    ( effortDetailedActivityId effortDetailed
    , \ activityId' -> effortDetailed { effortDetailedActivityId = activityId' }
    )
instance AthleteIdLens EffortDetailed (Integer) where
  athleteId effortDetailed =
    ( effortDetailedAthleteId effortDetailed
    , \ athleteId' -> effortDetailed { effortDetailedAthleteId = athleteId' }
    )
instance AverageCadenceLens EffortDetailed (Maybe Double) where
  averageCadence effortDetailed =
    ( effortDetailedAverageCadence effortDetailed
    , \ averageCadence' -> effortDetailed { effortDetailedAverageCadence = averageCadence' }
    )
instance AverageHeartrateLens EffortDetailed (Maybe Double) where
  averageHeartrate effortDetailed =
    ( effortDetailedAverageHeartrate effortDetailed
    , \ averageHeartrate' -> effortDetailed { effortDetailedAverageHeartrate = averageHeartrate' }
    )
instance AverageWattsLens EffortDetailed (Maybe Double) where
  averageWatts effortDetailed =
    ( effortDetailedAverageWatts effortDetailed
    , \ averageWatts' -> effortDetailed { effortDetailedAverageWatts = averageWatts' }
    )
instance DistanceLens EffortDetailed (Double) where
  distance effortDetailed =
    ( effortDetailedDistance effortDetailed
    , \ distance' -> effortDetailed { effortDetailedDistance = distance' }
    )
instance ElapsedTimeLens EffortDetailed (Integer) where
  elapsedTime effortDetailed =
    ( effortDetailedElapsedTime effortDetailed
    , \ elapsedTime' -> effortDetailed { effortDetailedElapsedTime = elapsedTime' }
    )
instance EndIndexLens EffortDetailed (Integer) where
  endIndex effortDetailed =
    ( effortDetailedEndIndex effortDetailed
    , \ endIndex' -> effortDetailed { effortDetailedEndIndex = endIndex' }
    )
instance HiddenLens EffortDetailed (Maybe Bool) where
  hidden effortDetailed =
    ( effortDetailedHidden effortDetailed
    , \ hidden' -> effortDetailed { effortDetailedHidden = hidden' }
    )
instance IdLens EffortDetailed (Integer) where
  id effortDetailed =
    ( effortDetailedId effortDetailed
    , \ id' -> effortDetailed { effortDetailedId = id' }
    )
instance KomRankLens EffortDetailed (Maybe Integer) where
  komRank effortDetailed =
    ( effortDetailedKomRank effortDetailed
    , \ komRank' -> effortDetailed { effortDetailedKomRank = komRank' }
    )
instance Max_heartrateLens EffortDetailed (Maybe Integer) where
  max_heartrate effortDetailed =
    ( effortDetailedMax_heartrate effortDetailed
    , \ max_heartrate' -> effortDetailed { effortDetailedMax_heartrate = max_heartrate' }
    )
instance MovingTimeLens EffortDetailed (Integer) where
  movingTime effortDetailed =
    ( effortDetailedMovingTime effortDetailed
    , \ movingTime' -> effortDetailed { effortDetailedMovingTime = movingTime' }
    )
instance NameLens EffortDetailed (Text) where
  name effortDetailed =
    ( effortDetailedName effortDetailed
    , \ name' -> effortDetailed { effortDetailedName = name' }
    )
instance PrRankLens EffortDetailed (Maybe Integer) where
  prRank effortDetailed =
    ( effortDetailedPrRank effortDetailed
    , \ prRank' -> effortDetailed { effortDetailedPrRank = prRank' }
    )
instance ResourceStateLens EffortDetailed (Integer) where
  resourceState effortDetailed =
    ( effortDetailedResourceState effortDetailed
    , \ resourceState' -> effortDetailed { effortDetailedResourceState = resourceState' }
    )
instance SegmentLens EffortDetailed (SegmentSummary) where
  segment effortDetailed =
    ( effortDetailedSegment effortDetailed
    , \ segment' -> effortDetailed { effortDetailedSegment = segment' }
    )
instance StartDateLens EffortDetailed (UTCTime) where
  startDate effortDetailed =
    ( effortDetailedStartDate effortDetailed
    , \ startDate' -> effortDetailed { effortDetailedStartDate = startDate' }
    )
instance StartDateLocalLens EffortDetailed (UTCTime) where
  startDateLocal effortDetailed =
    ( effortDetailedStartDateLocal effortDetailed
    , \ startDateLocal' -> effortDetailed { effortDetailedStartDateLocal = startDateLocal' }
    )
instance StartIndexLens EffortDetailed (Integer) where
  startIndex effortDetailed =
    ( effortDetailedStartIndex effortDetailed
    , \ startIndex' -> effortDetailed { effortDetailedStartIndex = startIndex' }
    )

-- ** GearDetailed

instance BrandNameLens GearDetailed (Text) where
  brandName gearDetailed =
    ( gearDetailedBrandName gearDetailed
    , \ brandName' -> gearDetailed { gearDetailedBrandName = brandName' }
    )
instance DescriptionLens GearDetailed (Text) where
  description gearDetailed =
    ( gearDetailedDescription gearDetailed
    , \ description' -> gearDetailed { gearDetailedDescription = description' }
    )
instance DistanceLens GearDetailed (Double) where
  distance gearDetailed =
    ( gearDetailedDistance gearDetailed
    , \ distance' -> gearDetailed { gearDetailedDistance = distance' }
    )
instance FrameTypeLens GearDetailed (Maybe Integer) where
  frameType gearDetailed =
    ( gearDetailedFrameType gearDetailed
    , \ frameType' -> gearDetailed { gearDetailedFrameType = frameType' }
    )
instance IdLens GearDetailed (Text) where
  id gearDetailed =
    ( gearDetailedId gearDetailed
    , \ id' -> gearDetailed { gearDetailedId = id' }
    )
instance ModelNameLens GearDetailed (Text) where
  modelName gearDetailed =
    ( gearDetailedModelName gearDetailed
    , \ modelName' -> gearDetailed { gearDetailedModelName = modelName' }
    )
instance NameLens GearDetailed (Text) where
  name gearDetailed =
    ( gearDetailedName gearDetailed
    , \ name' -> gearDetailed { gearDetailedName = name' }
    )
instance PrimaryLens GearDetailed (Bool) where
  primary gearDetailed =
    ( gearDetailedPrimary gearDetailed
    , \ primary' -> gearDetailed { gearDetailedPrimary = primary' }
    )
instance ResourceStateLens GearDetailed (Integer) where
  resourceState gearDetailed =
    ( gearDetailedResourceState gearDetailed
    , \ resourceState' -> gearDetailed { gearDetailedResourceState = resourceState' }
    )

-- ** GearSummary

instance DistanceLens GearSummary (Double) where
  distance gearSummary =
    ( gearSummaryDistance gearSummary
    , \ distance' -> gearSummary { gearSummaryDistance = distance' }
    )
instance IdLens GearSummary (Text) where
  id gearSummary =
    ( gearSummaryId gearSummary
    , \ id' -> gearSummary { gearSummaryId = id' }
    )
instance NameLens GearSummary (Text) where
  name gearSummary =
    ( gearSummaryName gearSummary
    , \ name' -> gearSummary { gearSummaryName = name' }
    )
instance PrimaryLens GearSummary (Bool) where
  primary gearSummary =
    ( gearSummaryPrimary gearSummary
    , \ primary' -> gearSummary { gearSummaryPrimary = primary' }
    )
instance ResourceStateLens GearSummary (Integer) where
  resourceState gearSummary =
    ( gearSummaryResourceState gearSummary
    , \ resourceState' -> gearSummary { gearSummaryResourceState = resourceState' }
    )

-- ** PhotoSummary

instance ActivityIdLens PhotoSummary (Integer) where
  activityId photoSummary =
    ( photoSummaryActivityId photoSummary
    , \ activityId' -> photoSummary { photoSummaryActivityId = activityId' }
    )
instance CaptionLens PhotoSummary (Text) where
  caption photoSummary =
    ( photoSummaryCaption photoSummary
    , \ caption' -> photoSummary { photoSummaryCaption = caption' }
    )
instance CreatedAtLens PhotoSummary (UTCTime) where
  createdAt photoSummary =
    ( photoSummaryCreatedAt photoSummary
    , \ createdAt' -> photoSummary { photoSummaryCreatedAt = createdAt' }
    )
instance IdLens PhotoSummary (Integer) where
  id photoSummary =
    ( photoSummaryId photoSummary
    , \ id' -> photoSummary { photoSummaryId = id' }
    )
instance LocationLens PhotoSummary (Maybe (Double, Double)) where
  location photoSummary =
    ( photoSummaryLocation photoSummary
    , \ location' -> photoSummary { photoSummaryLocation = location' }
    )
instance RefLens PhotoSummary (Text) where
  ref photoSummary =
    ( photoSummaryRef photoSummary
    , \ ref' -> photoSummary { photoSummaryRef = ref' }
    )
instance ResourceStateLens PhotoSummary (Integer) where
  resourceState photoSummary =
    ( photoSummaryResourceState photoSummary
    , \ resourceState' -> photoSummary { photoSummaryResourceState = resourceState' }
    )
instance TypeLens PhotoSummary (Text) where
  type' photoSummary =
    ( photoSummaryType photoSummary
    , \ type'' -> photoSummary { photoSummaryType = type'' }
    )
instance UidLens PhotoSummary (Text) where
  uid photoSummary =
    ( photoSummaryUid photoSummary
    , \ uid' -> photoSummary { photoSummaryUid = uid' }
    )
instance UploadedAtLens PhotoSummary (UTCTime) where
  uploadedAt photoSummary =
    ( photoSummaryUploadedAt photoSummary
    , \ uploadedAt' -> photoSummary { photoSummaryUploadedAt = uploadedAt' }
    )

-- ** PolylineDetailed

instance IdLens PolylineDetailed (Text) where
  id polylineDetailed =
    ( polylineDetailedId polylineDetailed
    , \ id' -> polylineDetailed { polylineDetailedId = id' }
    )
instance PolylineLens PolylineDetailed ([(Double, Double)]) where
  polyline polylineDetailed =
    ( polylineDetailedPolyline polylineDetailed
    , \ polyline' -> polylineDetailed { polylineDetailedPolyline = polyline' }
    )
instance ResourceStateLens PolylineDetailed (Integer) where
  resourceState polylineDetailed =
    ( polylineDetailedResourceState polylineDetailed
    , \ resourceState' -> polylineDetailed { polylineDetailedResourceState = resourceState' }
    )
instance SummaryPolylineLens PolylineDetailed (Maybe [(Double, Double)]) where
  summaryPolyline polylineDetailed =
    ( polylineDetailedSummaryPolyline polylineDetailed
    , \ summaryPolyline' -> polylineDetailed { polylineDetailedSummaryPolyline = summaryPolyline' }
    )

-- ** PolylineSummary

instance IdLens PolylineSummary (Text) where
  id polylineSummary =
    ( polylineSummaryId polylineSummary
    , \ id' -> polylineSummary { polylineSummaryId = id' }
    )
instance ResourceStateLens PolylineSummary (Integer) where
  resourceState polylineSummary =
    ( polylineSummaryResourceState polylineSummary
    , \ resourceState' -> polylineSummary { polylineSummaryResourceState = resourceState' }
    )
instance SummaryPolylineLens PolylineSummary (Maybe [(Double, Double)]) where
  summaryPolyline polylineSummary =
    ( polylineSummarySummaryPolyline polylineSummary
    , \ summaryPolyline' -> polylineSummary { polylineSummarySummaryPolyline = summaryPolyline' }
    )

-- ** SegmentDetailed

instance ActivityTypeLens SegmentDetailed (Text) where
  activityType segmentDetailed =
    ( segmentDetailedActivityType segmentDetailed
    , \ activityType' -> segmentDetailed { segmentDetailedActivityType = activityType' }
    )
instance AthleteCountLens SegmentDetailed (Integer) where
  athleteCount segmentDetailed =
    ( segmentDetailedAthleteCount segmentDetailed
    , \ athleteCount' -> segmentDetailed { segmentDetailedAthleteCount = athleteCount' }
    )
instance AverageGradeLens SegmentDetailed (Double) where
  averageGrade segmentDetailed =
    ( segmentDetailedAverageGrade segmentDetailed
    , \ averageGrade' -> segmentDetailed { segmentDetailedAverageGrade = averageGrade' }
    )
instance CityLens SegmentDetailed (Text) where
  city segmentDetailed =
    ( segmentDetailedCity segmentDetailed
    , \ city' -> segmentDetailed { segmentDetailedCity = city' }
    )
instance ClimbCategoryLens SegmentDetailed (Integer) where
  climbCategory segmentDetailed =
    ( segmentDetailedClimbCategory segmentDetailed
    , \ climbCategory' -> segmentDetailed { segmentDetailedClimbCategory = climbCategory' }
    )
instance CountryLens SegmentDetailed (Text) where
  country segmentDetailed =
    ( segmentDetailedCountry segmentDetailed
    , \ country' -> segmentDetailed { segmentDetailedCountry = country' }
    )
instance CreatedAtLens SegmentDetailed (UTCTime) where
  createdAt segmentDetailed =
    ( segmentDetailedCreatedAt segmentDetailed
    , \ createdAt' -> segmentDetailed { segmentDetailedCreatedAt = createdAt' }
    )
instance DistanceLens SegmentDetailed (Double) where
  distance segmentDetailed =
    ( segmentDetailedDistance segmentDetailed
    , \ distance' -> segmentDetailed { segmentDetailedDistance = distance' }
    )
instance EffortCountLens SegmentDetailed (Integer) where
  effortCount segmentDetailed =
    ( segmentDetailedEffortCount segmentDetailed
    , \ effortCount' -> segmentDetailed { segmentDetailedEffortCount = effortCount' }
    )
instance ElevationHighLens SegmentDetailed (Double) where
  elevationHigh segmentDetailed =
    ( segmentDetailedElevationHigh segmentDetailed
    , \ elevationHigh' -> segmentDetailed { segmentDetailedElevationHigh = elevationHigh' }
    )
instance ElevationLowLens SegmentDetailed (Double) where
  elevationLow segmentDetailed =
    ( segmentDetailedElevationLow segmentDetailed
    , \ elevationLow' -> segmentDetailed { segmentDetailedElevationLow = elevationLow' }
    )
instance EndLatitudeLens SegmentDetailed (Double) where
  endLatitude segmentDetailed =
    ( segmentDetailedEndLatitude segmentDetailed
    , \ endLatitude' -> segmentDetailed { segmentDetailedEndLatitude = endLatitude' }
    )
instance EndLatlngLens SegmentDetailed ((Double, Double)) where
  endLatlng segmentDetailed =
    ( segmentDetailedEndLatlng segmentDetailed
    , \ endLatlng' -> segmentDetailed { segmentDetailedEndLatlng = endLatlng' }
    )
instance EndLongitudeLens SegmentDetailed (Double) where
  endLongitude segmentDetailed =
    ( segmentDetailedEndLongitude segmentDetailed
    , \ endLongitude' -> segmentDetailed { segmentDetailedEndLongitude = endLongitude' }
    )
instance HazardousLens SegmentDetailed (Bool) where
  hazardous segmentDetailed =
    ( segmentDetailedHazardous segmentDetailed
    , \ hazardous' -> segmentDetailed { segmentDetailedHazardous = hazardous' }
    )
instance IdLens SegmentDetailed (Integer) where
  id segmentDetailed =
    ( segmentDetailedId segmentDetailed
    , \ id' -> segmentDetailed { segmentDetailedId = id' }
    )
instance MapLens SegmentDetailed (PolylineDetailed) where
  map segmentDetailed =
    ( segmentDetailedMap segmentDetailed
    , \ map' -> segmentDetailed { segmentDetailedMap = map' }
    )
instance MaximumGradeLens SegmentDetailed (Double) where
  maximumGrade segmentDetailed =
    ( segmentDetailedMaximumGrade segmentDetailed
    , \ maximumGrade' -> segmentDetailed { segmentDetailedMaximumGrade = maximumGrade' }
    )
instance NameLens SegmentDetailed (Text) where
  name segmentDetailed =
    ( segmentDetailedName segmentDetailed
    , \ name' -> segmentDetailed { segmentDetailedName = name' }
    )
instance PrivateLens SegmentDetailed (Bool) where
  private segmentDetailed =
    ( segmentDetailedPrivate segmentDetailed
    , \ private' -> segmentDetailed { segmentDetailedPrivate = private' }
    )
instance ResourceStateLens SegmentDetailed (Integer) where
  resourceState segmentDetailed =
    ( segmentDetailedResourceState segmentDetailed
    , \ resourceState' -> segmentDetailed { segmentDetailedResourceState = resourceState' }
    )
instance StarCountLens SegmentDetailed (Integer) where
  starCount segmentDetailed =
    ( segmentDetailedStarCount segmentDetailed
    , \ starCount' -> segmentDetailed { segmentDetailedStarCount = starCount' }
    )
instance StarredLens SegmentDetailed (Bool) where
  starred segmentDetailed =
    ( segmentDetailedStarred segmentDetailed
    , \ starred' -> segmentDetailed { segmentDetailedStarred = starred' }
    )
instance StartLatitudeLens SegmentDetailed (Double) where
  startLatitude segmentDetailed =
    ( segmentDetailedStartLatitude segmentDetailed
    , \ startLatitude' -> segmentDetailed { segmentDetailedStartLatitude = startLatitude' }
    )
instance StartLatlngLens SegmentDetailed ((Double, Double)) where
  startLatlng segmentDetailed =
    ( segmentDetailedStartLatlng segmentDetailed
    , \ startLatlng' -> segmentDetailed { segmentDetailedStartLatlng = startLatlng' }
    )
instance StartLongitudeLens SegmentDetailed (Double) where
  startLongitude segmentDetailed =
    ( segmentDetailedStartLongitude segmentDetailed
    , \ startLongitude' -> segmentDetailed { segmentDetailedStartLongitude = startLongitude' }
    )
instance StateLens SegmentDetailed (Text) where
  state segmentDetailed =
    ( segmentDetailedState segmentDetailed
    , \ state' -> segmentDetailed { segmentDetailedState = state' }
    )
instance TotalElevationGainLens SegmentDetailed (Double) where
  totalElevationGain segmentDetailed =
    ( segmentDetailedTotalElevationGain segmentDetailed
    , \ totalElevationGain' -> segmentDetailed { segmentDetailedTotalElevationGain = totalElevationGain' }
    )
instance UpdatedAtLens SegmentDetailed (UTCTime) where
  updatedAt segmentDetailed =
    ( segmentDetailedUpdatedAt segmentDetailed
    , \ updatedAt' -> segmentDetailed { segmentDetailedUpdatedAt = updatedAt' }
    )

-- ** SegmentExplorer

instance EntriesLens SegmentExplorer ([SegmentExplorerEntry]) where
  entries segmentExplorer =
    ( segmentExplorerEntries segmentExplorer
    , \ entries' -> segmentExplorer { segmentExplorerEntries = entries' }
    )

-- ** SegmentExplorerEntry

instance AvgGradeLens SegmentExplorerEntry (Double) where
  avgGrade segmentExplorerEntry =
    ( segmentExplorerEntryAvgGrade segmentExplorerEntry
    , \ avgGrade' -> segmentExplorerEntry { segmentExplorerEntryAvgGrade = avgGrade' }
    )
instance ClimbCategoryLens SegmentExplorerEntry (Integer) where
  climbCategory segmentExplorerEntry =
    ( segmentExplorerEntryClimbCategory segmentExplorerEntry
    , \ climbCategory' -> segmentExplorerEntry { segmentExplorerEntryClimbCategory = climbCategory' }
    )
instance ClimbCategoryDescLens SegmentExplorerEntry (String) where
  climbCategoryDesc segmentExplorerEntry =
    ( segmentExplorerEntryClimbCategoryDesc segmentExplorerEntry
    , \ climbCategoryDesc' -> segmentExplorerEntry { segmentExplorerEntryClimbCategoryDesc = climbCategoryDesc' }
    )
instance DistanceLens SegmentExplorerEntry (Double) where
  distance segmentExplorerEntry =
    ( segmentExplorerEntryDistance segmentExplorerEntry
    , \ distance' -> segmentExplorerEntry { segmentExplorerEntryDistance = distance' }
    )
instance ElevDifferenceLens SegmentExplorerEntry (Double) where
  elevDifference segmentExplorerEntry =
    ( segmentExplorerEntryElevDifference segmentExplorerEntry
    , \ elevDifference' -> segmentExplorerEntry { segmentExplorerEntryElevDifference = elevDifference' }
    )
instance EndLatlngLens SegmentExplorerEntry ((Double, Double)) where
  endLatlng segmentExplorerEntry =
    ( segmentExplorerEntryEndLatlng segmentExplorerEntry
    , \ endLatlng' -> segmentExplorerEntry { segmentExplorerEntryEndLatlng = endLatlng' }
    )
instance IdLens SegmentExplorerEntry (Integer) where
  id segmentExplorerEntry =
    ( segmentExplorerEntryId segmentExplorerEntry
    , \ id' -> segmentExplorerEntry { segmentExplorerEntryId = id' }
    )
instance NameLens SegmentExplorerEntry (Text) where
  name segmentExplorerEntry =
    ( segmentExplorerEntryName segmentExplorerEntry
    , \ name' -> segmentExplorerEntry { segmentExplorerEntryName = name' }
    )
instance PointsLens SegmentExplorerEntry (Text) where
  points segmentExplorerEntry =
    ( segmentExplorerEntryPoints segmentExplorerEntry
    , \ points' -> segmentExplorerEntry { segmentExplorerEntryPoints = points' }
    )
instance ResourceStateLens SegmentExplorerEntry (Integer) where
  resourceState segmentExplorerEntry =
    ( segmentExplorerEntryResourceState segmentExplorerEntry
    , \ resourceState' -> segmentExplorerEntry { segmentExplorerEntryResourceState = resourceState' }
    )
instance StarredLens SegmentExplorerEntry (Bool) where
  starred segmentExplorerEntry =
    ( segmentExplorerEntryStarred segmentExplorerEntry
    , \ starred' -> segmentExplorerEntry { segmentExplorerEntryStarred = starred' }
    )
instance StartLatlngLens SegmentExplorerEntry ((Double, Double)) where
  startLatlng segmentExplorerEntry =
    ( segmentExplorerEntryStartLatlng segmentExplorerEntry
    , \ startLatlng' -> segmentExplorerEntry { segmentExplorerEntryStartLatlng = startLatlng' }
    )

-- ** SegmentLeaderboard

instance EntriesLens SegmentLeaderboard ([SegmentLeaderboardEntry]) where
  entries segmentLeaderboard =
    ( segmentLeaderboardEntries segmentLeaderboard
    , \ entries' -> segmentLeaderboard { segmentLeaderboardEntries = entries' }
    )

-- ** SegmentLeaderboardEntry

instance ActivityIdLens SegmentLeaderboardEntry (Integer) where
  activityId segmentLeaderboardEntry =
    ( segmentLeaderboardEntryActivityId segmentLeaderboardEntry
    , \ activityId' -> segmentLeaderboardEntry { segmentLeaderboardEntryActivityId = activityId' }
    )
instance AthleteGenderLens SegmentLeaderboardEntry (Maybe Char) where
  athleteGender segmentLeaderboardEntry =
    ( segmentLeaderboardEntryAthleteGender segmentLeaderboardEntry
    , \ athleteGender' -> segmentLeaderboardEntry { segmentLeaderboardEntryAthleteGender = athleteGender' }
    )
instance AthleteIdLens SegmentLeaderboardEntry (Integer) where
  athleteId segmentLeaderboardEntry =
    ( segmentLeaderboardEntryAthleteId segmentLeaderboardEntry
    , \ athleteId' -> segmentLeaderboardEntry { segmentLeaderboardEntryAthleteId = athleteId' }
    )
instance AthleteNameLens SegmentLeaderboardEntry (Text) where
  athleteName segmentLeaderboardEntry =
    ( segmentLeaderboardEntryAthleteName segmentLeaderboardEntry
    , \ athleteName' -> segmentLeaderboardEntry { segmentLeaderboardEntryAthleteName = athleteName' }
    )
instance AthleteProfileLens SegmentLeaderboardEntry (Text) where
  athleteProfile segmentLeaderboardEntry =
    ( segmentLeaderboardEntryAthleteProfile segmentLeaderboardEntry
    , \ athleteProfile' -> segmentLeaderboardEntry { segmentLeaderboardEntryAthleteProfile = athleteProfile' }
    )
instance AverageHrLens SegmentLeaderboardEntry (Double) where
  averageHr segmentLeaderboardEntry =
    ( segmentLeaderboardEntryAverageHr segmentLeaderboardEntry
    , \ averageHr' -> segmentLeaderboardEntry { segmentLeaderboardEntryAverageHr = averageHr' }
    )
instance AverageWattsLens SegmentLeaderboardEntry (Double) where
  averageWatts segmentLeaderboardEntry =
    ( segmentLeaderboardEntryAverageWatts segmentLeaderboardEntry
    , \ averageWatts' -> segmentLeaderboardEntry { segmentLeaderboardEntryAverageWatts = averageWatts' }
    )
instance DistanceLens SegmentLeaderboardEntry (Double) where
  distance segmentLeaderboardEntry =
    ( segmentLeaderboardEntryDistance segmentLeaderboardEntry
    , \ distance' -> segmentLeaderboardEntry { segmentLeaderboardEntryDistance = distance' }
    )
instance EffortIdLens SegmentLeaderboardEntry (Integer) where
  effortId segmentLeaderboardEntry =
    ( segmentLeaderboardEntryEffortId segmentLeaderboardEntry
    , \ effortId' -> segmentLeaderboardEntry { segmentLeaderboardEntryEffortId = effortId' }
    )
instance ElapsedTimeLens SegmentLeaderboardEntry (Integer) where
  elapsedTime segmentLeaderboardEntry =
    ( segmentLeaderboardEntryElapsedTime segmentLeaderboardEntry
    , \ elapsedTime' -> segmentLeaderboardEntry { segmentLeaderboardEntryElapsedTime = elapsedTime' }
    )
instance MovingTimeLens SegmentLeaderboardEntry (Integer) where
  movingTime segmentLeaderboardEntry =
    ( segmentLeaderboardEntryMovingTime segmentLeaderboardEntry
    , \ movingTime' -> segmentLeaderboardEntry { segmentLeaderboardEntryMovingTime = movingTime' }
    )
instance RankLens SegmentLeaderboardEntry (Integer) where
  rank segmentLeaderboardEntry =
    ( segmentLeaderboardEntryRank segmentLeaderboardEntry
    , \ rank' -> segmentLeaderboardEntry { segmentLeaderboardEntryRank = rank' }
    )
instance StartDateLens SegmentLeaderboardEntry (UTCTime) where
  startDate segmentLeaderboardEntry =
    ( segmentLeaderboardEntryStartDate segmentLeaderboardEntry
    , \ startDate' -> segmentLeaderboardEntry { segmentLeaderboardEntryStartDate = startDate' }
    )
instance StartDateLocalLens SegmentLeaderboardEntry (UTCTime) where
  startDateLocal segmentLeaderboardEntry =
    ( segmentLeaderboardEntryStartDateLocal segmentLeaderboardEntry
    , \ startDateLocal' -> segmentLeaderboardEntry { segmentLeaderboardEntryStartDateLocal = startDateLocal' }
    )

-- ** SegmentSummary

instance ActivityTypeLens SegmentSummary (Text) where
  activityType segmentSummary =
    ( segmentSummaryActivityType segmentSummary
    , \ activityType' -> segmentSummary { segmentSummaryActivityType = activityType' }
    )
instance AverageGradeLens SegmentSummary (Double) where
  averageGrade segmentSummary =
    ( segmentSummaryAverageGrade segmentSummary
    , \ averageGrade' -> segmentSummary { segmentSummaryAverageGrade = averageGrade' }
    )
instance CityLens SegmentSummary (Text) where
  city segmentSummary =
    ( segmentSummaryCity segmentSummary
    , \ city' -> segmentSummary { segmentSummaryCity = city' }
    )
instance ClimbCategoryLens SegmentSummary (Integer) where
  climbCategory segmentSummary =
    ( segmentSummaryClimbCategory segmentSummary
    , \ climbCategory' -> segmentSummary { segmentSummaryClimbCategory = climbCategory' }
    )
instance CountryLens SegmentSummary (Text) where
  country segmentSummary =
    ( segmentSummaryCountry segmentSummary
    , \ country' -> segmentSummary { segmentSummaryCountry = country' }
    )
instance DistanceLens SegmentSummary (Double) where
  distance segmentSummary =
    ( segmentSummaryDistance segmentSummary
    , \ distance' -> segmentSummary { segmentSummaryDistance = distance' }
    )
instance ElevationHighLens SegmentSummary (Double) where
  elevationHigh segmentSummary =
    ( segmentSummaryElevationHigh segmentSummary
    , \ elevationHigh' -> segmentSummary { segmentSummaryElevationHigh = elevationHigh' }
    )
instance ElevationLowLens SegmentSummary (Double) where
  elevationLow segmentSummary =
    ( segmentSummaryElevationLow segmentSummary
    , \ elevationLow' -> segmentSummary { segmentSummaryElevationLow = elevationLow' }
    )
instance EndLatitudeLens SegmentSummary (Double) where
  endLatitude segmentSummary =
    ( segmentSummaryEndLatitude segmentSummary
    , \ endLatitude' -> segmentSummary { segmentSummaryEndLatitude = endLatitude' }
    )
instance EndLatlngLens SegmentSummary ((Double, Double)) where
  endLatlng segmentSummary =
    ( segmentSummaryEndLatlng segmentSummary
    , \ endLatlng' -> segmentSummary { segmentSummaryEndLatlng = endLatlng' }
    )
instance EndLongitudeLens SegmentSummary (Double) where
  endLongitude segmentSummary =
    ( segmentSummaryEndLongitude segmentSummary
    , \ endLongitude' -> segmentSummary { segmentSummaryEndLongitude = endLongitude' }
    )
instance IdLens SegmentSummary (Integer) where
  id segmentSummary =
    ( segmentSummaryId segmentSummary
    , \ id' -> segmentSummary { segmentSummaryId = id' }
    )
instance MaximumGradeLens SegmentSummary (Double) where
  maximumGrade segmentSummary =
    ( segmentSummaryMaximumGrade segmentSummary
    , \ maximumGrade' -> segmentSummary { segmentSummaryMaximumGrade = maximumGrade' }
    )
instance NameLens SegmentSummary (Text) where
  name segmentSummary =
    ( segmentSummaryName segmentSummary
    , \ name' -> segmentSummary { segmentSummaryName = name' }
    )
instance PrivateLens SegmentSummary (Bool) where
  private segmentSummary =
    ( segmentSummaryPrivate segmentSummary
    , \ private' -> segmentSummary { segmentSummaryPrivate = private' }
    )
instance ResourceStateLens SegmentSummary (Integer) where
  resourceState segmentSummary =
    ( segmentSummaryResourceState segmentSummary
    , \ resourceState' -> segmentSummary { segmentSummaryResourceState = resourceState' }
    )
instance StarredLens SegmentSummary (Bool) where
  starred segmentSummary =
    ( segmentSummaryStarred segmentSummary
    , \ starred' -> segmentSummary { segmentSummaryStarred = starred' }
    )
instance StartLatitudeLens SegmentSummary (Double) where
  startLatitude segmentSummary =
    ( segmentSummaryStartLatitude segmentSummary
    , \ startLatitude' -> segmentSummary { segmentSummaryStartLatitude = startLatitude' }
    )
instance StartLatlngLens SegmentSummary ((Double, Double)) where
  startLatlng segmentSummary =
    ( segmentSummaryStartLatlng segmentSummary
    , \ startLatlng' -> segmentSummary { segmentSummaryStartLatlng = startLatlng' }
    )
instance StartLongitudeLens SegmentSummary (Double) where
  startLongitude segmentSummary =
    ( segmentSummaryStartLongitude segmentSummary
    , \ startLongitude' -> segmentSummary { segmentSummaryStartLongitude = startLongitude' }
    )
instance StateLens SegmentSummary (Text) where
  state segmentSummary =
    ( segmentSummaryState segmentSummary
    , \ state' -> segmentSummary { segmentSummaryState = state' }
    )

-- ** StreamDetailed

instance DataLens StreamDetailed ([Value]) where
  data' streamDetailed =
    ( streamDetailedData streamDetailed
    , \ data'' -> streamDetailed { streamDetailedData = data'' }
    )
instance OriginalSizeLens StreamDetailed (Integer) where
  originalSize streamDetailed =
    ( streamDetailedOriginalSize streamDetailed
    , \ originalSize' -> streamDetailed { streamDetailedOriginalSize = originalSize' }
    )
instance ResolutionLens StreamDetailed (Text) where
  resolution streamDetailed =
    ( streamDetailedResolution streamDetailed
    , \ resolution' -> streamDetailed { streamDetailedResolution = resolution' }
    )
instance SeriesTypeLens StreamDetailed (Text) where
  seriesType streamDetailed =
    ( streamDetailedSeriesType streamDetailed
    , \ seriesType' -> streamDetailed { streamDetailedSeriesType = seriesType' }
    )
instance TypeLens StreamDetailed (Text) where
  type' streamDetailed =
    ( streamDetailedType streamDetailed
    , \ type'' -> streamDetailed { streamDetailedType = type'' }
    )

-- ** TokenExchangeResponse

instance AccessTokenLens TokenExchangeResponse (Text) where
  accessToken' tokenExchangeResponse =
    ( tokenExchangeResponseAccessToken tokenExchangeResponse
    , \ accessToken'' -> tokenExchangeResponse { tokenExchangeResponseAccessToken = accessToken'' }
    )
instance AthleteLens TokenExchangeResponse (AthleteDetailed) where
  athlete tokenExchangeResponse =
    ( tokenExchangeResponseAthlete tokenExchangeResponse
    , \ athlete' -> tokenExchangeResponse { tokenExchangeResponseAthlete = athlete' }
    )

-- ** UploadStatus

instance ActivityIdLens UploadStatus (Maybe Integer) where
  activityId uploadStatus =
    ( uploadStatusActivityId uploadStatus
    , \ activityId' -> uploadStatus { uploadStatusActivityId = activityId' }
    )
instance ErrorLens UploadStatus (Maybe Text) where
  error uploadStatus =
    ( uploadStatusError uploadStatus
    , \ error' -> uploadStatus { uploadStatusError = error' }
    )
instance ExternalIdLens UploadStatus (Text) where
  externalId uploadStatus =
    ( uploadStatusExternalId uploadStatus
    , \ externalId' -> uploadStatus { uploadStatusExternalId = externalId' }
    )
instance IdLens UploadStatus (Integer) where
  id uploadStatus =
    ( uploadStatusId uploadStatus
    , \ id' -> uploadStatus { uploadStatusId = id' }
    )
instance StatusLens UploadStatus (Text) where
  status uploadStatus =
    ( uploadStatusStatus uploadStatus
    , \ status' -> uploadStatus { uploadStatusStatus = status' }
    )
