-- | 'Strive.Actions.Activities'
module Strive.Options.Activities
  ( CreateActivityOptions (..),
    GetActivityOptions (..),
    UpdateActivityOptions (..),
    GetCurrentActivitiesOptions (..),
    GetRelatedActivitiesOptions,
    GetFeedOptions,
  )
where

import Data.Aeson (encode)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy (toStrict)
import Data.Default (Default, def)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Network.HTTP.Types (QueryLike, toQuery)
import Strive.Enums (ActivityType)
import Strive.Internal.Options (PaginationOptions)

-- | 'Strive.Actions.CreateActivity'
data CreateActivityOptions = CreateActivityOptions
  { createActivityOptions_description :: Maybe String,
    createActivityOptions_distance :: Maybe Double
  }
  deriving (Show)

instance Default CreateActivityOptions where
  def =
    CreateActivityOptions
      { createActivityOptions_description = Nothing,
        createActivityOptions_distance = Nothing
      }

instance QueryLike CreateActivityOptions where
  toQuery options =
    toQuery
      [ ("description", createActivityOptions_description options),
        ("distance", fmap show (createActivityOptions_distance options))
      ]

-- | 'Strive.Actions.GetActivity'
newtype GetActivityOptions = GetActivityOptions
  { getActivityOptions_allEfforts :: Bool
  }
  deriving (Show)

instance Default GetActivityOptions where
  def = GetActivityOptions {getActivityOptions_allEfforts = False}

instance QueryLike GetActivityOptions where
  toQuery options =
    toQuery
      [ ( "approval_prompt",
          unpack (toStrict (encode (getActivityOptions_allEfforts options)))
        )
      ]

-- | 'Strive.Actions.UpdateActivity'
data UpdateActivityOptions = UpdateActivityOptions
  { updateActivityOptions_name :: Maybe String,
    updateActivityOptions_type :: Maybe ActivityType,
    updateActivityOptions_private :: Maybe Bool,
    updateActivityOptions_commute :: Maybe Bool,
    updateActivityOptions_trainer :: Maybe Bool,
    updateActivityOptions_gearId :: Maybe String,
    updateActivityOptions_description :: Maybe String
  }
  deriving (Show)

instance Default UpdateActivityOptions where
  def =
    UpdateActivityOptions
      { updateActivityOptions_name = Nothing,
        updateActivityOptions_type = Nothing,
        updateActivityOptions_private = Nothing,
        updateActivityOptions_commute = Nothing,
        updateActivityOptions_trainer = Nothing,
        updateActivityOptions_gearId = Nothing,
        updateActivityOptions_description = Nothing
      }

instance QueryLike UpdateActivityOptions where
  toQuery options =
    toQuery
      [ ("name", updateActivityOptions_name options),
        ("type", fmap show (updateActivityOptions_type options)),
        ( "private",
          fmap
            (unpack . toStrict . encode)
            (updateActivityOptions_private options)
        ),
        ( "commute",
          fmap
            (unpack . toStrict . encode)
            (updateActivityOptions_commute options)
        ),
        ( "trainer",
          fmap
            (unpack . toStrict . encode)
            (updateActivityOptions_trainer options)
        ),
        ("gear_id", updateActivityOptions_gearId options),
        ("description", updateActivityOptions_description options)
      ]

-- | 'Strive.Actions.getCurrentActivities'
data GetCurrentActivitiesOptions = GetCurrentActivitiesOptions
  { getCurrentActivitiesOptions_before :: Maybe UTCTime,
    getCurrentActivitiesOptions_after :: Maybe UTCTime,
    getCurrentActivitiesOptions_page :: Integer,
    getCurrentActivitiesOptions_perPage :: Integer
  }
  deriving (Show)

instance Default GetCurrentActivitiesOptions where
  def =
    GetCurrentActivitiesOptions
      { getCurrentActivitiesOptions_before = Nothing,
        getCurrentActivitiesOptions_after = Nothing,
        getCurrentActivitiesOptions_page = 1,
        getCurrentActivitiesOptions_perPage = 200
      }

instance QueryLike GetCurrentActivitiesOptions where
  toQuery options =
    toQuery
      [ ( "before",
          fmap
            (show . utcTimeToPOSIXSeconds)
            (getCurrentActivitiesOptions_before options)
        ),
        ( "after",
          fmap
            (show . utcTimeToPOSIXSeconds)
            (getCurrentActivitiesOptions_after options)
        ),
        ("page", Just (show (getCurrentActivitiesOptions_page options))),
        ("per_page", Just (show (getCurrentActivitiesOptions_perPage options)))
      ]

-- | 'Strive.Actions.getRelatedActivities'
type GetRelatedActivitiesOptions = PaginationOptions

-- | 'Strive.Actions.getFeed'
type GetFeedOptions = PaginationOptions
