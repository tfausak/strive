{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Strive.Types where

import Control.Applicative (empty, (<$>), (<*>))
import Data.Aeson (FromJSON, Value (Object), parseJSON, (.:), (.:?))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Strive.Lenses

-- * Authentication

-- | <http://strava.github.io/api/v3/oauth/#example-response>
data TokenExchangeResponse = TokenExchangeResponse
  { tokenExchangeResponse_accessToken :: Text
  , tokenExchangeResponse_athlete     :: AthleteDetailed
  } deriving Show

instance FromJSON TokenExchangeResponse where
  parseJSON (Object o) = TokenExchangeResponse
    <$> o .: "access_token"
    <*> o .: "athlete"
  parseJSON _ = empty

-- | <http://strava.github.io/api/v3/oauth/#example-response-1>
data DeauthorizationResponse = DeauthorizationResponse
  { deauthorizationResponse_accessToken :: Text
  } deriving Show

instance FromJSON DeauthorizationResponse where
  parseJSON (Object o) = DeauthorizationResponse
    <$> o .: "access_token"
  parseJSON _ = empty

-- * Athletes

-- | <http://strava.github.io/api/v3/athlete/#detailed>
data AthleteDetailed = AthleteDetailed
  { athleteDetailed_bikes                 :: [GearSummary]
  , athleteDetailed_city                  :: Text
  , athleteDetailed_clubs                 :: [ClubSummary]
  , athleteDetailed_country               :: Text
  , athleteDetailed_createdAt             :: UTCTime
  , athleteDetailed_datePreference        :: Text
  , athleteDetailed_email                 :: Text
  , athleteDetailed_firstname             :: Text
  , athleteDetailed_follower              :: Maybe Text
  , athleteDetailed_followerCount         :: Integer
  , athleteDetailed_friend                :: Maybe Text
  , athleteDetailed_friendCount           :: Integer
  , athleteDetailed_ftp                   :: Maybe Integer
  , athleteDetailed_id                    :: Integer
  , athleteDetailed_lastname              :: Text
  , athleteDetailed_measurementPreference :: Text
  , athleteDetailed_mutualFriendCount     :: Integer
  , athleteDetailed_premium               :: Bool
  , athleteDetailed_profile               :: Text
  , athleteDetailed_profileMedium         :: Text
  , athleteDetailed_resourceState         :: Integer
  , athleteDetailed_sex                   :: Maybe Char
  , athleteDetailed_shoes                 :: [GearSummary]
  , athleteDetailed_state                 :: Text
  , athleteDetailed_updatedAt             :: UTCTime
  } deriving Show

instance FromJSON AthleteDetailed where
  parseJSON (Object o) = AthleteDetailed
    <$> o .: "bikes"
    <*> o .: "city"
    <*> o .: "clubs"
    <*> o .: "country"
    <*> o .: "created_at"
    <*> o .: "date_preference"
    <*> o .: "email"
    <*> o .: "firstname"
    <*> o .:? "follower"
    <*> o .: "follower_count"
    <*> o .:? "friend"
    <*> o .: "friend_count"
    <*> o .:? "ftp"
    <*> o .: "id"
    <*> o .: "lastname"
    <*> o .: "measurement_preference"
    <*> o .: "mutual_friend_count"
    <*> o .: "premium"
    <*> o .: "profile"
    <*> o .: "profile_medium"
    <*> o .: "resource_state"
    <*> o .:? "sex"
    <*> o .: "shoes"
    <*> o .: "state"
    <*> o .: "updated_at"
  parseJSON _ = empty

-- * Clubs

-- | <http://strava.github.io/api/v3/clubs/#summary>
data ClubSummary = ClubSummary
  { clubSummary_id            :: Integer
  , clubSummary_name          :: Text
  , clubSummary_profile       :: Text
  , clubSummary_profileMedium :: Text
  , clubSummary_resourceState :: Integer
  } deriving Show

instance FromJSON ClubSummary where
  parseJSON (Object o) = ClubSummary
    <$> o .: "id"
    <*> o .: "name"
    <*> o .: "profile"
    <*> o .: "profile_medium"
    <*> o .: "resource_state"
  parseJSON _ = empty

-- * Gear

-- | <http://strava.github.io/api/v3/gear/#summary>
data GearSummary = GearSummary
  { gearSummary_distance      :: Double
  , gearSummary_id            :: Text
  , gearSummary_name          :: Text
  , gearSummary_primary       :: Bool
  , gearSummary_resourceState :: Integer
  } deriving Show

instance FromJSON GearSummary where
  parseJSON (Object o) = GearSummary
    <$> o .: "distance"
    <*> o .: "id"
    <*> o .: "name"
    <*> o .: "primary"
    <*> o .: "resource_state"
  parseJSON _ = empty

-- TODO

instance AccessTokenLens TokenExchangeResponse Text where
  accessToken tokenExchangeResponse =
    ( tokenExchangeResponse_accessToken tokenExchangeResponse
    , \ accessToken' -> tokenExchangeResponse { tokenExchangeResponse_accessToken = accessToken' }
    )
instance AthleteLens TokenExchangeResponse AthleteDetailed where
  athlete tokenExchangeResponse =
    ( tokenExchangeResponse_athlete tokenExchangeResponse
    , \ athlete' -> tokenExchangeResponse { tokenExchangeResponse_athlete = athlete' }
    )

instance AccessTokenLens DeauthorizationResponse (Text) where
  accessToken deauthorizationResponse =
    ( deauthorizationResponse_accessToken deauthorizationResponse
    , \ accessToken' -> deauthorizationResponse { deauthorizationResponse_accessToken = accessToken' }
    )

instance BikesLens AthleteDetailed ([GearSummary]) where
  bikes athleteDetailed =
    ( athleteDetailed_bikes athleteDetailed
    , \ bikes' -> athleteDetailed { athleteDetailed_bikes = bikes' }
    )
instance CityLens AthleteDetailed (Text) where
  city athleteDetailed =
    ( athleteDetailed_city athleteDetailed
    , \ city' -> athleteDetailed { athleteDetailed_city = city' }
    )
instance ClubsLens AthleteDetailed ([ClubSummary]) where
  clubs athleteDetailed =
    ( athleteDetailed_clubs athleteDetailed
    , \ clubs' -> athleteDetailed { athleteDetailed_clubs = clubs' }
    )
instance CountryLens AthleteDetailed (Text) where
  country athleteDetailed =
    ( athleteDetailed_country athleteDetailed
    , \ country' -> athleteDetailed { athleteDetailed_country = country' }
    )
instance CreatedAtLens AthleteDetailed (UTCTime) where
  createdAt athleteDetailed =
    ( athleteDetailed_createdAt athleteDetailed
    , \ createdAt' -> athleteDetailed { athleteDetailed_createdAt = createdAt' }
    )
instance DatePreferenceLens AthleteDetailed (Text) where
  datePreference athleteDetailed =
    ( athleteDetailed_datePreference athleteDetailed
    , \ datePreference' -> athleteDetailed { athleteDetailed_datePreference = datePreference' }
    )
instance EmailLens AthleteDetailed (Text) where
  email athleteDetailed =
    ( athleteDetailed_email athleteDetailed
    , \ email' -> athleteDetailed { athleteDetailed_email = email' }
    )
instance FirstnameLens AthleteDetailed (Text) where
  firstname athleteDetailed =
    ( athleteDetailed_firstname athleteDetailed
    , \ firstname' -> athleteDetailed { athleteDetailed_firstname = firstname' }
    )
instance FollowerLens AthleteDetailed (Maybe Text) where
  follower athleteDetailed =
    ( athleteDetailed_follower athleteDetailed
    , \ follower' -> athleteDetailed { athleteDetailed_follower = follower' }
    )
instance FollowerCountLens AthleteDetailed (Integer) where
  followerCount athleteDetailed =
    ( athleteDetailed_followerCount athleteDetailed
    , \ followerCount' -> athleteDetailed { athleteDetailed_followerCount = followerCount' }
    )
instance FriendLens AthleteDetailed (Maybe Text) where
  friend athleteDetailed =
    ( athleteDetailed_friend athleteDetailed
    , \ friend' -> athleteDetailed { athleteDetailed_friend = friend' }
    )
instance FriendCountLens AthleteDetailed (Integer) where
  friendCount athleteDetailed =
    ( athleteDetailed_friendCount athleteDetailed
    , \ friendCount' -> athleteDetailed { athleteDetailed_friendCount = friendCount' }
    )
instance FtpLens AthleteDetailed (Maybe Integer) where
  ftp athleteDetailed =
    ( athleteDetailed_ftp athleteDetailed
    , \ ftp' -> athleteDetailed { athleteDetailed_ftp = ftp' }
    )
instance IdLens AthleteDetailed (Integer) where
  id athleteDetailed =
    ( athleteDetailed_id athleteDetailed
    , \ id' -> athleteDetailed { athleteDetailed_id = id' }
    )
instance LastnameLens AthleteDetailed (Text) where
  lastname athleteDetailed =
    ( athleteDetailed_lastname athleteDetailed
    , \ lastname' -> athleteDetailed { athleteDetailed_lastname = lastname' }
    )
instance MeasurementPreferenceLens AthleteDetailed (Text) where
  measurementPreference athleteDetailed =
    ( athleteDetailed_measurementPreference athleteDetailed
    , \ measurementPreference' -> athleteDetailed { athleteDetailed_measurementPreference = measurementPreference' }
    )
instance MutualFriendCountLens AthleteDetailed (Integer) where
  mutualFriendCount athleteDetailed =
    ( athleteDetailed_mutualFriendCount athleteDetailed
    , \ mutualFriendCount' -> athleteDetailed { athleteDetailed_mutualFriendCount = mutualFriendCount' }
    )
instance PremiumLens AthleteDetailed (Bool) where
  premium athleteDetailed =
    ( athleteDetailed_premium athleteDetailed
    , \ premium' -> athleteDetailed { athleteDetailed_premium = premium' }
    )
instance ProfileLens AthleteDetailed (Text) where
  profile athleteDetailed =
    ( athleteDetailed_profile athleteDetailed
    , \ profile' -> athleteDetailed { athleteDetailed_profile = profile' }
    )
instance ProfileMediumLens AthleteDetailed (Text) where
  profileMedium athleteDetailed =
    ( athleteDetailed_profileMedium athleteDetailed
    , \ profileMedium' -> athleteDetailed { athleteDetailed_profileMedium = profileMedium' }
    )
instance ResourceStateLens AthleteDetailed (Integer) where
  resourceState athleteDetailed =
    ( athleteDetailed_resourceState athleteDetailed
    , \ resourceState' -> athleteDetailed { athleteDetailed_resourceState = resourceState' }
    )
instance SexLens AthleteDetailed (Maybe Char) where
  sex athleteDetailed =
    ( athleteDetailed_sex athleteDetailed
    , \ sex' -> athleteDetailed { athleteDetailed_sex = sex' }
    )
instance ShoesLens AthleteDetailed ([GearSummary]) where
  shoes athleteDetailed =
    ( athleteDetailed_shoes athleteDetailed
    , \ shoes' -> athleteDetailed { athleteDetailed_shoes = shoes' }
    )
instance StateLens AthleteDetailed (Text) where
  state athleteDetailed =
    ( athleteDetailed_state athleteDetailed
    , \ state' -> athleteDetailed { athleteDetailed_state = state' }
    )
instance UpdatedAtLens AthleteDetailed (UTCTime) where
  updatedAt athleteDetailed =
    ( athleteDetailed_updatedAt athleteDetailed
    , \ updatedAt' -> athleteDetailed { athleteDetailed_updatedAt = updatedAt' }
    )

instance IdLens ClubSummary (Integer) where
  id clubSummary =
    ( clubSummary_id clubSummary
    , \ id' -> clubSummary { clubSummary_id = id' }
    )
instance NameLens ClubSummary (Text) where
  name clubSummary =
    ( clubSummary_name clubSummary
    , \ name' -> clubSummary { clubSummary_name = name' }
    )
instance ProfileLens ClubSummary (Text) where
  profile clubSummary =
    ( clubSummary_profile clubSummary
    , \ profile' -> clubSummary { clubSummary_profile = profile' }
    )
instance ProfileMediumLens ClubSummary (Text) where
  profileMedium clubSummary =
    ( clubSummary_profileMedium clubSummary
    , \ profileMedium' -> clubSummary { clubSummary_profileMedium = profileMedium' }
    )
instance ResourceStateLens ClubSummary (Integer) where
  resourceState clubSummary =
    ( clubSummary_resourceState clubSummary
    , \ resourceState' -> clubSummary { clubSummary_resourceState = resourceState' }
    )

instance DistanceLens GearSummary (Double) where
  distance gearSummary =
    ( gearSummary_distance gearSummary
    , \ distance' -> gearSummary { gearSummary_distance = distance' }
    )
instance IdLens GearSummary (Text) where
  id gearSummary =
    ( gearSummary_id gearSummary
    , \ id' -> gearSummary { gearSummary_id = id' }
    )
instance NameLens GearSummary (Text) where
  name gearSummary =
    ( gearSummary_name gearSummary
    , \ name' -> gearSummary { gearSummary_name = name' }
    )
instance PrimaryLens GearSummary (Bool) where
  primary gearSummary =
    ( gearSummary_primary gearSummary
    , \ primary' -> gearSummary { gearSummary_primary = primary' }
    )
instance ResourceStateLens GearSummary (Integer) where
  resourceState gearSummary =
    ( gearSummary_resourceState gearSummary
    , \ resourceState' -> gearSummary { gearSummary_resourceState = resourceState' }
    )
