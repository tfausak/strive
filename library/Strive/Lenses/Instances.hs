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

instance ApprovalPromptLens BuildAuthorizeUrlOptions Bool where
  approvalPrompt buildAuthorizeUrlOptions =
    ( buildAuthorizeUrlOptions_approvalPrompt buildAuthorizeUrlOptions
    , \ approvalPrompt' -> buildAuthorizeUrlOptions { buildAuthorizeUrlOptions_approvalPrompt = approvalPrompt' }
    )

instance AthleteIdLens EffortDetailed Integer where
  athleteId effortDetailed =
    ( effortDetailed_athleteId effortDetailed
    , \ athleteId' -> effortDetailed { effortDetailed_athleteId = athleteId' }
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

instance AverageWattsLens EffortDetailed (Maybe Double) where
  averageWatts effortDetailed =
    ( effortDetailed_averageWatts effortDetailed
    , \ averageWatts' -> effortDetailed { effortDetailed_averageWatts = averageWatts' }
    )

instance BikesLens AthleteDetailed [GearSummary] where
  bikes athleteDetailed =
    ( athleteDetailed_bikes athleteDetailed
    , \ bikes' -> athleteDetailed { athleteDetailed_bikes = bikes' }
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

instance IdLens AthleteDetailed Integer where
  id athleteDetailed =
    ( athleteDetailed_id athleteDetailed
    , \ id' -> athleteDetailed { athleteDetailed_id = id' }
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

instance IdLens SegmentSummary Integer where
  id segmentSummary =
    ( segmentSummary_id segmentSummary
    , \ id' -> segmentSummary { segmentSummary_id = id' }
    )

instance KomRankLens EffortDetailed (Maybe Integer) where
  komRank effortDetailed =
    ( effortDetailed_komRank effortDetailed
    , \ komRank' -> effortDetailed { effortDetailed_komRank = komRank' }
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

instance MaxHeartrateLens EffortDetailed (Maybe Integer) where
  maxHeartrate effortDetailed =
    ( effortDetailed_maxHeartrate effortDetailed
    , \ maxHeartrate' -> effortDetailed { effortDetailed_maxHeartrate = maxHeartrate' }
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

instance PageLens GetAthleteCrsOptions Integer where
  page getAthleteCrsOptions =
    ( getAthleteCrsOptions_page getAthleteCrsOptions
    , \ page' -> getAthleteCrsOptions { getAthleteCrsOptions_page = page' }
    )

instance PageLens GetCurrentFriendsOptions Integer where
  page getCurrentFriendsOptions =
    ( getCurrentFriendsOptions_page getCurrentFriendsOptions
    , \ page' -> getCurrentFriendsOptions { getCurrentFriendsOptions_page = page' }
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

instance PerPageLens GetCurrentFriendsOptions Integer where
  perPage getCurrentFriendsOptions =
    ( getCurrentFriendsOptions_perPage getCurrentFriendsOptions
    , \ perPage' -> getCurrentFriendsOptions { getCurrentFriendsOptions_perPage = perPage' }
    )

instance PerPageLens GetFriendsOptions Integer where
  perPage getFriendsOptions =
    ( getFriendsOptions_perPage getFriendsOptions
    , \ perPage' -> getFriendsOptions { getFriendsOptions_perPage = perPage' }
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

instance PrivateLens SegmentSummary Bool where
  private segmentSummary =
    ( segmentSummary_private segmentSummary
    , \ private' -> segmentSummary { segmentSummary_private = private' }
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

instance ResourceStateLens AthleteDetailed Integer where
  resourceState athleteDetailed =
    ( athleteDetailed_resourceState athleteDetailed
    , \ resourceState' -> athleteDetailed { athleteDetailed_resourceState = resourceState' }
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

instance ResourceStateLens SegmentSummary Integer where
  resourceState segmentSummary =
    ( segmentSummary_resourceState segmentSummary
    , \ resourceState' -> segmentSummary { segmentSummary_resourceState = resourceState' }
    )

instance SegmentLens EffortDetailed SegmentSummary where
  segment effortDetailed =
    ( effortDetailed_segment effortDetailed
    , \ segment' -> effortDetailed { effortDetailed_segment = segment' }
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

instance StartDateLens EffortDetailed UTCTime where
  startDate effortDetailed =
    ( effortDetailed_startDate effortDetailed
    , \ startDate' -> effortDetailed { effortDetailed_startDate = startDate' }
    )

instance StartDateLocalLens EffortDetailed UTCTime where
  startDateLocal effortDetailed =
    ( effortDetailed_startDateLocal effortDetailed
    , \ startDateLocal' -> effortDetailed { effortDetailed_startDateLocal = startDateLocal' }
    )

instance StartIndexLens EffortDetailed Integer where
  startIndex effortDetailed =
    ( effortDetailed_startIndex effortDetailed
    , \ startIndex' -> effortDetailed { effortDetailed_startIndex = startIndex' }
    )

instance StartLatitudeLens SegmentSummary Double where
  startLatitude segmentSummary =
    ( segmentSummary_startLatitude segmentSummary
    , \ startLatitude' -> segmentSummary { segmentSummary_startLatitude = startLatitude' }
    )

instance StartLatlngLens SegmentSummary ((Double, Double)) where
  startLatlng segmentSummary =
    ( segmentSummary_startLatlng segmentSummary
    , \ startLatlng' -> segmentSummary { segmentSummary_startLatlng = startLatlng' }
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
