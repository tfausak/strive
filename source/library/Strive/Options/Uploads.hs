-- | 'Strive.Actions.Uploads'
module Strive.Options.Uploads
  ( UploadActivityOptions(..)
  ) where

import Data.Default (Default, def)
import Network.HTTP.Types (QueryLike, toQuery)
import Strive.Enums (ActivityType)

-- | 'Strive.Actions.uploadActivity'
data UploadActivityOptions = UploadActivityOptions
  { uploadActivityOptions_activityType :: Maybe ActivityType
  , uploadActivityOptions_name :: Maybe String
  , uploadActivityOptions_description :: Maybe String
  , uploadActivityOptions_private :: Bool
  , uploadActivityOptions_trainer :: Bool
  , uploadActivityOptions_externalId :: Maybe String
  }
  deriving Show

instance Default UploadActivityOptions where
  def = UploadActivityOptions
    { uploadActivityOptions_activityType = Nothing
    , uploadActivityOptions_name = Nothing
    , uploadActivityOptions_description = Nothing
    , uploadActivityOptions_private = False
    , uploadActivityOptions_trainer = False
    , uploadActivityOptions_externalId = Nothing
    }

instance QueryLike UploadActivityOptions where
  toQuery options = toQuery
    [ ("activity_type", fmap show (uploadActivityOptions_activityType options))
    , ("name", uploadActivityOptions_name options)
    , ("description", uploadActivityOptions_description options)
    , ( "private"
      , Just (show (fromEnum (uploadActivityOptions_private options)))
      )
    , ( "trainer"
      , Just (show (fromEnum (uploadActivityOptions_trainer options)))
      )
    , ("external_id", uploadActivityOptions_externalId options)
    ]
