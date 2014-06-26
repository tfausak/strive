{-# LANGUAGE OverloadedStrings #-}

-- | Helper functions for performing actions.
module Strive.Utilities
    ( buildQuery
    , buildRequest
    , buildURL
    , decodeResponse
    , get
    , makeRequest
    , paginate
    , queryToSimpleQuery
    , renderQuery
    ) where

import           Data.Aeson             (FromJSON, eitherDecode)
import           Data.ByteString.Char8  (pack, unpack)
import           Data.ByteString.Lazy   (ByteString)
import           Data.Monoid            ((<>))
import           Network.HTTP.Conduit   (Request, Response, checkStatus,
                                         httpLbs, parseUrl, responseBody)
import           Network.HTTP.Types.URI (Query, SimpleQuery, renderSimpleQuery)
import           Strive.Client          (Client (accessToken, httpManager))
import           Strive.Types           (Page, PerPage, Resource)

-- | Build a base query with just the access token for a client.
buildQuery :: Client -> SimpleQuery
buildQuery client =
    [ ("access_token", pack (accessToken client))
    ]

-- | Build a request by constructing the URL and appending the access token.
buildRequest :: Client -> Resource -> SimpleQuery -> IO Request
buildRequest client resource query = do
    request <- parseUrl (buildURL client resource query)
    return request
        { checkStatus = \ _ _ _ -> Nothing
        }

-- | Build a URL for a request.
buildURL :: Client -> Resource -> SimpleQuery -> String
buildURL client resource query = concat
    [ "https://www.strava.com/api/v3/"
    , resource
    , renderQuery (buildQuery client <> query)
    ]

-- | Decode a response by parsing its body as JSON.
decodeResponse :: FromJSON a => Response ByteString -> Either String a
decodeResponse = eitherDecode . responseBody

-- | Get the given resource.
get :: FromJSON a => Client -> Resource -> SimpleQuery -> IO (Either String a)
get client resource query = do
    request <- buildRequest client resource query
    response <- makeRequest client request
    return (decodeResponse response)

-- | Make an HTTP request using the client's manager.
makeRequest :: Client -> Request -> IO (Response ByteString)
makeRequest = flip httpLbs . httpManager

-- | Convert pagination parameters into a query.
paginate :: Page -> PerPage -> SimpleQuery
paginate page perPage = queryToSimpleQuery
    [ ("page", fmap (pack . show) page)
    , ("per_page", fmap (pack . show) perPage)
    ]

-- | Convert a query into a simple query.
queryToSimpleQuery :: Query -> SimpleQuery
queryToSimpleQuery = foldr go []
  where
    go (_, Nothing) query = query
    go (key, Just value) query = (key, value) : query

-- | Render a simple query as a string prefixed by a question mark.
renderQuery :: SimpleQuery -> String
renderQuery = unpack . renderSimpleQuery True
