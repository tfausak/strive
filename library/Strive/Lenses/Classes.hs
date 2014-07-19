{-# LANGUAGE FunctionalDependencies #-}

-- | Automatically generated lens classes.
module Strive.Lenses.Classes
  ( AccessTokenLens (..)
  , AchievementCountLens (..)
  , ActivityIdLens (..)
  , ActivityTypeLens (..)
  , AfterLens (..)
  , AgeGroupLens (..)
  , AllEffortsLens (..)
  , ApprovalPromptLens (..)
  , AthleteCountLens (..)
  , AthleteGenderLens (..)
  , AthleteIdLens (..)
  , AthleteLens (..)
  , AthleteNameLens (..)
  , AthleteProfileLens (..)
  , AverageCadenceLens (..)
  , AverageGradeLens (..)
  , AverageHeartrateLens (..)
  , AverageHrLens (..)
  , AverageSpeedLens (..)
  , AverageWattsLens (..)
  , AvgGradeLens (..)
  , BeforeLens (..)
  , BikesLens (..)
  , BrandNameLens (..)
  , CaloriesLens (..)
  , CaptionLens (..)
  , CityLens (..)
  , ClimbCategoryDescLens (..)
  , ClimbCategoryLens (..)
  , ClubIdLens (..)
  , ClubTypeLens (..)
  , ClubsLens (..)
  , CommentCountLens (..)
  , CommuteLens (..)
  , CountryLens (..)
  , CreatedAtLens (..)
  , DataLens (..)
  , DatePreferenceLens (..)
  , DateRangeLens (..)
  , DescriptionLens (..)
  , DistanceLens (..)
  , DistributionBucketsLens (..)
  , EffortCountLens (..)
  , EffortIdLens (..)
  , ElapsedTimeLens (..)
  , ElevDifferenceLens (..)
  , ElevationHighLens (..)
  , ElevationLowLens (..)
  , EmailLens (..)
  , EndIndexLens (..)
  , EndLatitudeLens (..)
  , EndLatlngLens (..)
  , EndLongitudeLens (..)
  , EntriesLens (..)
  , EntryCountLens (..)
  , ErrorLens (..)
  , ExternalIdLens (..)
  , FirstnameLens (..)
  , FlaggedLens (..)
  , FollowerCountLens (..)
  , FollowerLens (..)
  , FollowingLens (..)
  , FrameTypeLens (..)
  , FriendCountLens (..)
  , FriendLens (..)
  , FtpLens (..)
  , GearIdLens (..)
  , GearLens (..)
  , GenderLens (..)
  , HasKudoedLens (..)
  , HazardousLens (..)
  , HiddenLens (..)
  , IdLens (..)
  , InstagramPrimaryPhotoLens (..)
  , KilojoulesLens (..)
  , KomRankLens (..)
  , KudosCountLens (..)
  , LapIndexLens (..)
  , LastnameLens (..)
  , LocationCityLens (..)
  , LocationCountryLens (..)
  , LocationLens (..)
  , LocationStateLens (..)
  , ManualLens (..)
  , MapLens (..)
  , MarkdownLens (..)
  , MaxCatLens (..)
  , MaxHeartrateLens (..)
  , MaxLens (..)
  , MaxSpeedLens (..)
  , MaximumGradeLens (..)
  , MeasurementPreferenceLens (..)
  , MemberCountLens (..)
  , MinCatLens (..)
  , MinLens (..)
  , ModelNameLens (..)
  , MovingTimeLens (..)
  , MutualFriendCountLens (..)
  , NameLens (..)
  , OriginalSizeLens (..)
  , PageLens (..)
  , PerPageLens (..)
  , PhotoCountLens (..)
  , PointsLens (..)
  , PolylineLens (..)
  , PrRankLens (..)
  , PremiumLens (..)
  , PrimaryLens (..)
  , PrivateLens (..)
  , PrivateScopeLens (..)
  , ProfileLens (..)
  , ProfileMediumLens (..)
  , RangeLens (..)
  , RankLens (..)
  , RefLens (..)
  , RequesterLens (..)
  , ResolutionLens (..)
  , ResourceStateLens (..)
  , SegmentEffortsLens (..)
  , SegmentLens (..)
  , SensorBasedLens (..)
  , SeriesTypeLens (..)
  , SexLens (..)
  , ShoesLens (..)
  , SportTypeLens (..)
  , StarCountLens (..)
  , StarredLens (..)
  , StartDateLens (..)
  , StartDateLocalLens (..)
  , StartIndexLens (..)
  , StartLatitudeLens (..)
  , StartLatlngLens (..)
  , StartLongitudeLens (..)
  , StateLens (..)
  , StatusLens (..)
  , SummaryPolylineLens (..)
  , TextLens (..)
  , TimeLens (..)
  , TimezoneLens (..)
  , TotalElevationGainLens (..)
  , TrainerLens (..)
  , TruncatedLens (..)
  , TypeLens (..)
  , UidLens (..)
  , UpdatedAtLens (..)
  , UploadIdLens (..)
  , UploadedAtLens (..)
  , WeightClassLens (..)
  , WeightLens (..)
  , WriteScopeLens (..)
  ) where

import Strive.Lenses (Lens)

-- | Class for the 'accessToken' lens.
class AccessTokenLens a b | a -> b where
  accessToken :: Lens a b

-- | Class for the 'achievementCount' lens.
class AchievementCountLens a b | a -> b where
  achievementCount :: Lens a b

-- | Class for the 'activityId' lens.
class ActivityIdLens a b | a -> b where
  activityId :: Lens a b

-- | Class for the 'activityType' lens.
class ActivityTypeLens a b | a -> b where
  activityType :: Lens a b

-- | Class for the 'after' lens.
class AfterLens a b | a -> b where
  after :: Lens a b

-- | Class for the 'ageGroup' lens.
class AgeGroupLens a b | a -> b where
  ageGroup :: Lens a b

-- | Class for the 'allEfforts' lens.
class AllEffortsLens a b | a -> b where
  allEfforts :: Lens a b

-- | Class for the 'approvalPrompt' lens.
class ApprovalPromptLens a b | a -> b where
  approvalPrompt :: Lens a b

-- | Class for the 'athleteCount' lens.
class AthleteCountLens a b | a -> b where
  athleteCount :: Lens a b

-- | Class for the 'athleteGender' lens.
class AthleteGenderLens a b | a -> b where
  athleteGender :: Lens a b

-- | Class for the 'athleteId' lens.
class AthleteIdLens a b | a -> b where
  athleteId :: Lens a b

-- | Class for the 'athlete' lens.
class AthleteLens a b | a -> b where
  athlete :: Lens a b

-- | Class for the 'athleteName' lens.
class AthleteNameLens a b | a -> b where
  athleteName :: Lens a b

-- | Class for the 'athleteProfile' lens.
class AthleteProfileLens a b | a -> b where
  athleteProfile :: Lens a b

-- | Class for the 'averageCadence' lens.
class AverageCadenceLens a b | a -> b where
  averageCadence :: Lens a b

-- | Class for the 'averageGrade' lens.
class AverageGradeLens a b | a -> b where
  averageGrade :: Lens a b

-- | Class for the 'averageHeartrate' lens.
class AverageHeartrateLens a b | a -> b where
  averageHeartrate :: Lens a b

-- | Class for the 'averageHr' lens.
class AverageHrLens a b | a -> b where
  averageHr :: Lens a b

-- | Class for the 'averageSpeed' lens.
class AverageSpeedLens a b | a -> b where
  averageSpeed :: Lens a b

-- | Class for the 'averageWatts' lens.
class AverageWattsLens a b | a -> b where
  averageWatts :: Lens a b

-- | Class for the 'avgGrade' lens.
class AvgGradeLens a b | a -> b where
  avgGrade :: Lens a b

-- | Class for the 'before' lens.
class BeforeLens a b | a -> b where
  before :: Lens a b

-- | Class for the 'bikes' lens.
class BikesLens a b | a -> b where
  bikes :: Lens a b

-- | Class for the 'brandName' lens.
class BrandNameLens a b | a -> b where
  brandName :: Lens a b

-- | Class for the 'calories' lens.
class CaloriesLens a b | a -> b where
  calories :: Lens a b

-- | Class for the 'caption' lens.
class CaptionLens a b | a -> b where
  caption :: Lens a b

-- | Class for the 'city' lens.
class CityLens a b | a -> b where
  city :: Lens a b

-- | Class for the 'climbCategoryDesc' lens.
class ClimbCategoryDescLens a b | a -> b where
  climbCategoryDesc :: Lens a b

-- | Class for the 'climbCategory' lens.
class ClimbCategoryLens a b | a -> b where
  climbCategory :: Lens a b

-- | Class for the 'clubId' lens.
class ClubIdLens a b | a -> b where
  clubId :: Lens a b

-- | Class for the 'clubType' lens.
class ClubTypeLens a b | a -> b where
  clubType :: Lens a b

-- | Class for the 'clubs' lens.
class ClubsLens a b | a -> b where
  clubs :: Lens a b

-- | Class for the 'commentCount' lens.
class CommentCountLens a b | a -> b where
  commentCount :: Lens a b

-- | Class for the 'commute' lens.
class CommuteLens a b | a -> b where
  commute :: Lens a b

-- | Class for the 'country' lens.
class CountryLens a b | a -> b where
  country :: Lens a b

-- | Class for the 'createdAt' lens.
class CreatedAtLens a b | a -> b where
  createdAt :: Lens a b

-- | Class for the 'data_' lens.
class DataLens a b | a -> b where
  data_ :: Lens a b

-- | Class for the 'datePreference' lens.
class DatePreferenceLens a b | a -> b where
  datePreference :: Lens a b

-- | Class for the 'dateRange' lens.
class DateRangeLens a b | a -> b where
  dateRange :: Lens a b

-- | Class for the 'description' lens.
class DescriptionLens a b | a -> b where
  description :: Lens a b

-- | Class for the 'distance' lens.
class DistanceLens a b | a -> b where
  distance :: Lens a b

-- | Class for the 'distributionBuckets' lens.
class DistributionBucketsLens a b | a -> b where
  distributionBuckets :: Lens a b

-- | Class for the 'effortCount' lens.
class EffortCountLens a b | a -> b where
  effortCount :: Lens a b

-- | Class for the 'effortId' lens.
class EffortIdLens a b | a -> b where
  effortId :: Lens a b

-- | Class for the 'elapsedTime' lens.
class ElapsedTimeLens a b | a -> b where
  elapsedTime :: Lens a b

-- | Class for the 'elevDifference' lens.
class ElevDifferenceLens a b | a -> b where
  elevDifference :: Lens a b

-- | Class for the 'elevationHigh' lens.
class ElevationHighLens a b | a -> b where
  elevationHigh :: Lens a b

-- | Class for the 'elevationLow' lens.
class ElevationLowLens a b | a -> b where
  elevationLow :: Lens a b

-- | Class for the 'email' lens.
class EmailLens a b | a -> b where
  email :: Lens a b

-- | Class for the 'endIndex' lens.
class EndIndexLens a b | a -> b where
  endIndex :: Lens a b

-- | Class for the 'endLatitude' lens.
class EndLatitudeLens a b | a -> b where
  endLatitude :: Lens a b

-- | Class for the 'endLatlng' lens.
class EndLatlngLens a b | a -> b where
  endLatlng :: Lens a b

-- | Class for the 'endLongitude' lens.
class EndLongitudeLens a b | a -> b where
  endLongitude :: Lens a b

-- | Class for the 'entries' lens.
class EntriesLens a b | a -> b where
  entries :: Lens a b

-- | Class for the 'entryCount' lens.
class EntryCountLens a b | a -> b where
  entryCount :: Lens a b

-- | Class for the 'error_' lens.
class ErrorLens a b | a -> b where
  error_ :: Lens a b

-- | Class for the 'externalId' lens.
class ExternalIdLens a b | a -> b where
  externalId :: Lens a b

-- | Class for the 'firstname' lens.
class FirstnameLens a b | a -> b where
  firstname :: Lens a b

-- | Class for the 'flagged' lens.
class FlaggedLens a b | a -> b where
  flagged :: Lens a b

-- | Class for the 'followerCount' lens.
class FollowerCountLens a b | a -> b where
  followerCount :: Lens a b

-- | Class for the 'follower' lens.
class FollowerLens a b | a -> b where
  follower :: Lens a b

-- | Class for the 'following' lens.
class FollowingLens a b | a -> b where
  following :: Lens a b

-- | Class for the 'frameType' lens.
class FrameTypeLens a b | a -> b where
  frameType :: Lens a b

-- | Class for the 'friendCount' lens.
class FriendCountLens a b | a -> b where
  friendCount :: Lens a b

-- | Class for the 'friend' lens.
class FriendLens a b | a -> b where
  friend :: Lens a b

-- | Class for the 'ftp' lens.
class FtpLens a b | a -> b where
  ftp :: Lens a b

-- | Class for the 'gearId' lens.
class GearIdLens a b | a -> b where
  gearId :: Lens a b

-- | Class for the 'gear' lens.
class GearLens a b | a -> b where
  gear :: Lens a b

-- | Class for the 'gender' lens.
class GenderLens a b | a -> b where
  gender :: Lens a b

-- | Class for the 'hasKudoed' lens.
class HasKudoedLens a b | a -> b where
  hasKudoed :: Lens a b

-- | Class for the 'hazardous' lens.
class HazardousLens a b | a -> b where
  hazardous :: Lens a b

-- | Class for the 'hidden' lens.
class HiddenLens a b | a -> b where
  hidden :: Lens a b

-- | Class for the 'id_' lens.
class IdLens a b | a -> b where
  id_ :: Lens a b

-- | Class for the 'instagramPrimaryPhoto' lens.
class InstagramPrimaryPhotoLens a b | a -> b where
  instagramPrimaryPhoto :: Lens a b

-- | Class for the 'kilojoules' lens.
class KilojoulesLens a b | a -> b where
  kilojoules :: Lens a b

-- | Class for the 'komRank' lens.
class KomRankLens a b | a -> b where
  komRank :: Lens a b

-- | Class for the 'kudosCount' lens.
class KudosCountLens a b | a -> b where
  kudosCount :: Lens a b

-- | Class for the 'lapIndex' lens.
class LapIndexLens a b | a -> b where
  lapIndex :: Lens a b

-- | Class for the 'lastname' lens.
class LastnameLens a b | a -> b where
  lastname :: Lens a b

-- | Class for the 'locationCity' lens.
class LocationCityLens a b | a -> b where
  locationCity :: Lens a b

-- | Class for the 'locationCountry' lens.
class LocationCountryLens a b | a -> b where
  locationCountry :: Lens a b

-- | Class for the 'location' lens.
class LocationLens a b | a -> b where
  location :: Lens a b

-- | Class for the 'locationState' lens.
class LocationStateLens a b | a -> b where
  locationState :: Lens a b

-- | Class for the 'manual' lens.
class ManualLens a b | a -> b where
  manual :: Lens a b

-- | Class for the 'map' lens.
class MapLens a b | a -> b where
  map :: Lens a b

-- | Class for the 'markdown' lens.
class MarkdownLens a b | a -> b where
  markdown :: Lens a b

-- | Class for the 'maxCat' lens.
class MaxCatLens a b | a -> b where
  maxCat :: Lens a b

-- | Class for the 'maxHeartrate' lens.
class MaxHeartrateLens a b | a -> b where
  maxHeartrate :: Lens a b

-- | Class for the 'max_' lens.
class MaxLens a b | a -> b where
  max_ :: Lens a b

-- | Class for the 'maxSpeed' lens.
class MaxSpeedLens a b | a -> b where
  maxSpeed :: Lens a b

-- | Class for the 'maximumGrade' lens.
class MaximumGradeLens a b | a -> b where
  maximumGrade :: Lens a b

-- | Class for the 'measurementPreference' lens.
class MeasurementPreferenceLens a b | a -> b where
  measurementPreference :: Lens a b

-- | Class for the 'memberCount' lens.
class MemberCountLens a b | a -> b where
  memberCount :: Lens a b

-- | Class for the 'minCat' lens.
class MinCatLens a b | a -> b where
  minCat :: Lens a b

-- | Class for the 'min_' lens.
class MinLens a b | a -> b where
  min_ :: Lens a b

-- | Class for the 'modelName' lens.
class ModelNameLens a b | a -> b where
  modelName :: Lens a b

-- | Class for the 'movingTime' lens.
class MovingTimeLens a b | a -> b where
  movingTime :: Lens a b

-- | Class for the 'mutualFriendCount' lens.
class MutualFriendCountLens a b | a -> b where
  mutualFriendCount :: Lens a b

-- | Class for the 'name' lens.
class NameLens a b | a -> b where
  name :: Lens a b

-- | Class for the 'originalSize' lens.
class OriginalSizeLens a b | a -> b where
  originalSize :: Lens a b

-- | Class for the 'page' lens.
class PageLens a b | a -> b where
  page :: Lens a b

-- | Class for the 'perPage' lens.
class PerPageLens a b | a -> b where
  perPage :: Lens a b

-- | Class for the 'photoCount' lens.
class PhotoCountLens a b | a -> b where
  photoCount :: Lens a b

-- | Class for the 'points' lens.
class PointsLens a b | a -> b where
  points :: Lens a b

-- | Class for the 'polyline' lens.
class PolylineLens a b | a -> b where
  polyline :: Lens a b

-- | Class for the 'prRank' lens.
class PrRankLens a b | a -> b where
  prRank :: Lens a b

-- | Class for the 'premium' lens.
class PremiumLens a b | a -> b where
  premium :: Lens a b

-- | Class for the 'primary' lens.
class PrimaryLens a b | a -> b where
  primary :: Lens a b

-- | Class for the 'private' lens.
class PrivateLens a b | a -> b where
  private :: Lens a b

-- | Class for the 'privateScope' lens.
class PrivateScopeLens a b | a -> b where
  privateScope :: Lens a b

-- | Class for the 'profile' lens.
class ProfileLens a b | a -> b where
  profile :: Lens a b

-- | Class for the 'profileMedium' lens.
class ProfileMediumLens a b | a -> b where
  profileMedium :: Lens a b

-- | Class for the 'range' lens.
class RangeLens a b | a -> b where
  range :: Lens a b

-- | Class for the 'rank' lens.
class RankLens a b | a -> b where
  rank :: Lens a b

-- | Class for the 'ref' lens.
class RefLens a b | a -> b where
  ref :: Lens a b

-- | Class for the 'requester' lens.
class RequesterLens a b | a -> b where
  requester :: Lens a b

-- | Class for the 'resolution' lens.
class ResolutionLens a b | a -> b where
  resolution :: Lens a b

-- | Class for the 'resourceState' lens.
class ResourceStateLens a b | a -> b where
  resourceState :: Lens a b

-- | Class for the 'segmentEfforts' lens.
class SegmentEffortsLens a b | a -> b where
  segmentEfforts :: Lens a b

-- | Class for the 'segment' lens.
class SegmentLens a b | a -> b where
  segment :: Lens a b

-- | Class for the 'sensorBased' lens.
class SensorBasedLens a b | a -> b where
  sensorBased :: Lens a b

-- | Class for the 'seriesType' lens.
class SeriesTypeLens a b | a -> b where
  seriesType :: Lens a b

-- | Class for the 'sex' lens.
class SexLens a b | a -> b where
  sex :: Lens a b

-- | Class for the 'shoes' lens.
class ShoesLens a b | a -> b where
  shoes :: Lens a b

-- | Class for the 'sportType' lens.
class SportTypeLens a b | a -> b where
  sportType :: Lens a b

-- | Class for the 'starCount' lens.
class StarCountLens a b | a -> b where
  starCount :: Lens a b

-- | Class for the 'starred' lens.
class StarredLens a b | a -> b where
  starred :: Lens a b

-- | Class for the 'startDate' lens.
class StartDateLens a b | a -> b where
  startDate :: Lens a b

-- | Class for the 'startDateLocal' lens.
class StartDateLocalLens a b | a -> b where
  startDateLocal :: Lens a b

-- | Class for the 'startIndex' lens.
class StartIndexLens a b | a -> b where
  startIndex :: Lens a b

-- | Class for the 'startLatitude' lens.
class StartLatitudeLens a b | a -> b where
  startLatitude :: Lens a b

-- | Class for the 'startLatlng' lens.
class StartLatlngLens a b | a -> b where
  startLatlng :: Lens a b

-- | Class for the 'startLongitude' lens.
class StartLongitudeLens a b | a -> b where
  startLongitude :: Lens a b

-- | Class for the 'state' lens.
class StateLens a b | a -> b where
  state :: Lens a b

-- | Class for the 'status' lens.
class StatusLens a b | a -> b where
  status :: Lens a b

-- | Class for the 'summaryPolyline' lens.
class SummaryPolylineLens a b | a -> b where
  summaryPolyline :: Lens a b

-- | Class for the 'text' lens.
class TextLens a b | a -> b where
  text :: Lens a b

-- | Class for the 'time' lens.
class TimeLens a b | a -> b where
  time :: Lens a b

-- | Class for the 'timezone' lens.
class TimezoneLens a b | a -> b where
  timezone :: Lens a b

-- | Class for the 'totalElevationGain' lens.
class TotalElevationGainLens a b | a -> b where
  totalElevationGain :: Lens a b

-- | Class for the 'trainer' lens.
class TrainerLens a b | a -> b where
  trainer :: Lens a b

-- | Class for the 'truncated' lens.
class TruncatedLens a b | a -> b where
  truncated :: Lens a b

-- | Class for the 'type_' lens.
class TypeLens a b | a -> b where
  type_ :: Lens a b

-- | Class for the 'uid' lens.
class UidLens a b | a -> b where
  uid :: Lens a b

-- | Class for the 'updatedAt' lens.
class UpdatedAtLens a b | a -> b where
  updatedAt :: Lens a b

-- | Class for the 'uploadId' lens.
class UploadIdLens a b | a -> b where
  uploadId :: Lens a b

-- | Class for the 'uploadedAt' lens.
class UploadedAtLens a b | a -> b where
  uploadedAt :: Lens a b

-- | Class for the 'weightClass' lens.
class WeightClassLens a b | a -> b where
  weightClass :: Lens a b

-- | Class for the 'weight' lens.
class WeightLens a b | a -> b where
  weight :: Lens a b

-- | Class for the 'writeScope' lens.
class WriteScopeLens a b | a -> b where
  writeScope :: Lens a b
