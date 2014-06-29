{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/v3/uploads/>
module Strive.Actions.Uploads
    ( getUpload
    , postUpload
    ) where

import           Data.ByteString.Char8 (ByteString, pack)
import           Data.Monoid           ((<>))
import           Network.HTTP.Conduit  (RequestBody (RequestBodyBS),
                                        requestBody)
import           Prelude               hiding (readFile)
import           Strive.Client         (Client, get)
import           Strive.Objects        (UploadDetailed)
import           Strive.Utilities      (buildRequest, decodeResponse,
                                        makeRequest, queryToSimpleQuery)

-- | <http://strava.github.io/api/v3/uploads/#get-status>
getUpload :: Client -> Integer -> IO (Either String UploadDetailed)
getUpload client uploadId = get client resource query
  where
    resource = "uploads/" <> show uploadId
    query = [] :: [(String, String)]

-- | <http://strava.github.io/api/v3/uploads/#post-file>
postUpload :: Client -> ByteString -> String -> Maybe String -> Maybe String -> Maybe String -> Maybe Integer -> Maybe Integer -> Maybe String -> IO (Either String UploadDetailed)
postUpload client body dataType activityType name description private trainer externalId = do
    initialRequest <- buildRequest client resource query
    let request = initialRequest
            { requestBody = RequestBodyBS body
            }
    response <- makeRequest client request
    return (decodeResponse response)
  where
    resource = "uploads"
    query = queryToSimpleQuery
        [ ("data_type", Just (pack dataType))
        , ("activity_type", fmap pack activityType)
        , ("name", fmap pack name)
        , ("description", fmap pack description)
        , ("private", fmap (pack . show) private)
        , ("trainer", fmap (pack . show) trainer)
        , ("external_id", fmap pack externalId)
        ]
