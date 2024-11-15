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
import qualified Data.Monoid as Monoid
import qualified Data.Semigroup as Semigroup
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Network.HTTP.Types (QueryLike, toQuery)
import Strive.Enums (ActivityType)
import Strive.Internal.Options (PaginationOptions)

-- | 'Strive.Actions.CreateActivity'
data CreateActivityOptions = CreateActivityOptions
  { createActivityOptions_description :: Monoid.Last String,
    createActivityOptions_distance :: Monoid.Last Double
  }
  deriving (Show)

instance Semigroup CreateActivityOptions where
  x <> y =
    CreateActivityOptions
      { createActivityOptions_description = createActivityOptions_description x <> createActivityOptions_description y,
        createActivityOptions_distance = createActivityOptions_distance x <> createActivityOptions_distance y
      }

instance Monoid CreateActivityOptions where
  mempty =
    CreateActivityOptions
      { createActivityOptions_description = mempty,
        createActivityOptions_distance = mempty
      }

instance QueryLike CreateActivityOptions where
  toQuery options =
    toQuery
      [ ("description", Monoid.getLast (createActivityOptions_description options)),
        ("distance", fmap show (Monoid.getLast (createActivityOptions_distance options)))
      ]

-- | 'Strive.Actions.GetActivity'
newtype GetActivityOptions = GetActivityOptions
  { getActivityOptions_allEfforts :: Semigroup.Last Bool
  }
  deriving (Show)

instance Semigroup GetActivityOptions where
  x <> y =
    GetActivityOptions
      { getActivityOptions_allEfforts = getActivityOptions_allEfforts x <> getActivityOptions_allEfforts y
      }

instance Monoid GetActivityOptions where
  mempty =
    GetActivityOptions
      { getActivityOptions_allEfforts = pure False
      }

instance QueryLike GetActivityOptions where
  toQuery options =
    toQuery
      [ ( "approval_prompt",
          unpack (toStrict (encode (Semigroup.getLast (getActivityOptions_allEfforts options))))
        )
      ]

-- | 'Strive.Actions.UpdateActivity'
data UpdateActivityOptions = UpdateActivityOptions
  { updateActivityOptions_name :: Monoid.Last String,
    updateActivityOptions_type :: Monoid.Last ActivityType,
    updateActivityOptions_private :: Monoid.Last Bool,
    updateActivityOptions_commute :: Monoid.Last Bool,
    updateActivityOptions_trainer :: Monoid.Last Bool,
    updateActivityOptions_gearId :: Monoid.Last String,
    updateActivityOptions_description :: Monoid.Last String
  }
  deriving (Show)

instance Semigroup UpdateActivityOptions where
  x <> y =
    UpdateActivityOptions
      { updateActivityOptions_name = updateActivityOptions_name x <> updateActivityOptions_name y,
        updateActivityOptions_type = updateActivityOptions_type x <> updateActivityOptions_type y,
        updateActivityOptions_private = updateActivityOptions_private x <> updateActivityOptions_private y,
        updateActivityOptions_commute = updateActivityOptions_commute x <> updateActivityOptions_commute y,
        updateActivityOptions_trainer = updateActivityOptions_trainer x <> updateActivityOptions_trainer y,
        updateActivityOptions_gearId = updateActivityOptions_gearId x <> updateActivityOptions_gearId y,
        updateActivityOptions_description = updateActivityOptions_description x <> updateActivityOptions_description y
      }

instance Monoid UpdateActivityOptions where
  mempty =
    UpdateActivityOptions
      { updateActivityOptions_name = mempty,
        updateActivityOptions_type = mempty,
        updateActivityOptions_private = mempty,
        updateActivityOptions_commute = mempty,
        updateActivityOptions_trainer = mempty,
        updateActivityOptions_gearId = mempty,
        updateActivityOptions_description = mempty
      }

instance QueryLike UpdateActivityOptions where
  toQuery options =
    toQuery
      [ ("name", Monoid.getLast (updateActivityOptions_name options)),
        ("type", fmap show (Monoid.getLast (updateActivityOptions_type options))),
        ( "private",
          fmap
            (unpack . toStrict . encode)
            (Monoid.getLast (updateActivityOptions_private options))
        ),
        ( "commute",
          fmap
            (unpack . toStrict . encode)
            (Monoid.getLast (updateActivityOptions_commute options))
        ),
        ( "trainer",
          fmap
            (unpack . toStrict . encode)
            (Monoid.getLast (updateActivityOptions_trainer options))
        ),
        ("gear_id", Monoid.getLast (updateActivityOptions_gearId options)),
        ("description", Monoid.getLast (updateActivityOptions_description options))
      ]

-- | 'Strive.Actions.getCurrentActivities'
data GetCurrentActivitiesOptions = GetCurrentActivitiesOptions
  { getCurrentActivitiesOptions_before :: Monoid.Last UTCTime,
    getCurrentActivitiesOptions_after :: Monoid.Last UTCTime,
    getCurrentActivitiesOptions_page :: Semigroup.Last Integer,
    getCurrentActivitiesOptions_perPage :: Semigroup.Last Integer
  }
  deriving (Show)

instance Semigroup GetCurrentActivitiesOptions where
  x <> y =
    GetCurrentActivitiesOptions
      { getCurrentActivitiesOptions_before = getCurrentActivitiesOptions_before x <> getCurrentActivitiesOptions_before y,
        getCurrentActivitiesOptions_after = getCurrentActivitiesOptions_after x <> getCurrentActivitiesOptions_after y,
        getCurrentActivitiesOptions_page = getCurrentActivitiesOptions_page x <> getCurrentActivitiesOptions_page y,
        getCurrentActivitiesOptions_perPage = getCurrentActivitiesOptions_perPage x <> getCurrentActivitiesOptions_perPage y
      }

instance Monoid GetCurrentActivitiesOptions where
  mempty =
    GetCurrentActivitiesOptions
      { getCurrentActivitiesOptions_before = mempty,
        getCurrentActivitiesOptions_after = mempty,
        getCurrentActivitiesOptions_page = pure 1,
        getCurrentActivitiesOptions_perPage = pure 200
      }

instance QueryLike GetCurrentActivitiesOptions where
  toQuery options =
    toQuery
      [ ( "before",
          fmap
            (show . utcTimeToPOSIXSeconds)
            (Monoid.getLast (getCurrentActivitiesOptions_before options))
        ),
        ( "after",
          fmap
            (show . utcTimeToPOSIXSeconds)
            (Monoid.getLast (getCurrentActivitiesOptions_after options))
        ),
        ("page", Just (show (Semigroup.getLast (getCurrentActivitiesOptions_page options)))),
        ("per_page", Just (show (Semigroup.getLast (getCurrentActivitiesOptions_perPage options))))
      ]

-- | 'Strive.Actions.getRelatedActivities'
type GetRelatedActivitiesOptions = PaginationOptions

-- | 'Strive.Actions.getFeed'
type GetFeedOptions = PaginationOptions
