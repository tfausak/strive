-- | 'Strive.Actions.Uploads'
module Strive.Options.Uploads
  ( UploadActivityOptions (..),
  )
where

import qualified Data.Monoid as Monoid
import Network.HTTP.Types (QueryLike, toQuery)
import Strive.Enums (ActivityType)

-- | 'Strive.Actions.uploadActivity'
data UploadActivityOptions = UploadActivityOptions
  { uploadActivityOptions_activityType :: Monoid.Last ActivityType,
    uploadActivityOptions_name :: Monoid.Last String,
    uploadActivityOptions_description :: Monoid.Last String,
    uploadActivityOptions_private :: Monoid.Last Bool,
    uploadActivityOptions_trainer :: Monoid.Last Bool,
    uploadActivityOptions_externalId :: Monoid.Last String
  }
  deriving (Show)

instance Semigroup UploadActivityOptions where
  x <> y =
    UploadActivityOptions
      { uploadActivityOptions_activityType = uploadActivityOptions_activityType x <> uploadActivityOptions_activityType y,
        uploadActivityOptions_name = uploadActivityOptions_name x <> uploadActivityOptions_name y,
        uploadActivityOptions_description = uploadActivityOptions_description x <> uploadActivityOptions_description y,
        uploadActivityOptions_private = uploadActivityOptions_private x <> uploadActivityOptions_private y,
        uploadActivityOptions_trainer = uploadActivityOptions_trainer x <> uploadActivityOptions_trainer y,
        uploadActivityOptions_externalId = uploadActivityOptions_externalId x <> uploadActivityOptions_externalId y
      }

instance Monoid UploadActivityOptions where
  mempty =
    UploadActivityOptions
      { uploadActivityOptions_activityType = mempty,
        uploadActivityOptions_name = mempty,
        uploadActivityOptions_description = mempty,
        uploadActivityOptions_private = mempty,
        uploadActivityOptions_trainer = mempty,
        uploadActivityOptions_externalId = mempty
      }

instance QueryLike UploadActivityOptions where
  toQuery options =
    toQuery
      [ fmap ((,) "activity_type" . show) . Monoid.getLast $ uploadActivityOptions_activityType options,
        fmap ((,) "name") . Monoid.getLast $ uploadActivityOptions_name options,
        fmap ((,) "description") . Monoid.getLast $ uploadActivityOptions_description options,
        fmap ((,) "private" . show) . Monoid.getLast $ uploadActivityOptions_private options,
        fmap ((,) "trainer" . show) . Monoid.getLast $ uploadActivityOptions_trainer options,
        fmap ((,) "external_id") . Monoid.getLast $ uploadActivityOptions_externalId options
      ]
