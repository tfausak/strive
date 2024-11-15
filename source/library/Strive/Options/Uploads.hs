-- | 'Strive.Actions.Uploads'
module Strive.Options.Uploads
  ( UploadActivityOptions (..),
  )
where

import qualified Data.Monoid as Monoid
import qualified Data.Semigroup as Semigroup
import Network.HTTP.Types (QueryLike, toQuery)
import Strive.Enums (ActivityType)

-- | 'Strive.Actions.uploadActivity'
data UploadActivityOptions = UploadActivityOptions
  { uploadActivityOptions_activityType :: Monoid.Last ActivityType,
    uploadActivityOptions_name :: Monoid.Last String,
    uploadActivityOptions_description :: Monoid.Last String,
    uploadActivityOptions_private :: Semigroup.Last Bool,
    uploadActivityOptions_trainer :: Semigroup.Last Bool,
    uploadActivityOptions_externalId :: Monoid.Last String
  }
  deriving (Show)

instance Semigroup.Semigroup UploadActivityOptions where
  x <> y =
    UploadActivityOptions
      { uploadActivityOptions_activityType = uploadActivityOptions_activityType x <> uploadActivityOptions_activityType y,
        uploadActivityOptions_name = uploadActivityOptions_name x <> uploadActivityOptions_name y,
        uploadActivityOptions_description = uploadActivityOptions_description x <> uploadActivityOptions_description y,
        uploadActivityOptions_private = uploadActivityOptions_private x <> uploadActivityOptions_private y,
        uploadActivityOptions_trainer = uploadActivityOptions_trainer x <> uploadActivityOptions_trainer y,
        uploadActivityOptions_externalId = uploadActivityOptions_externalId x <> uploadActivityOptions_externalId y
      }

instance Monoid.Monoid UploadActivityOptions where
  mempty =
    UploadActivityOptions
      { uploadActivityOptions_activityType = mempty,
        uploadActivityOptions_name = mempty,
        uploadActivityOptions_description = mempty,
        uploadActivityOptions_private = pure False,
        uploadActivityOptions_trainer = pure False,
        uploadActivityOptions_externalId = mempty
      }

instance QueryLike UploadActivityOptions where
  toQuery options =
    toQuery
      [ ("activity_type", fmap show (Monoid.getLast (uploadActivityOptions_activityType options))),
        ("name", Monoid.getLast (uploadActivityOptions_name options)),
        ("description", Monoid.getLast (uploadActivityOptions_description options)),
        ( "private",
          Just (show (fromEnum (Semigroup.getLast (uploadActivityOptions_private options))))
        ),
        ( "trainer",
          Just (show (fromEnum (Semigroup.getLast (uploadActivityOptions_trainer options))))
        ),
        ("external_id", Monoid.getLast (uploadActivityOptions_externalId options))
      ]
