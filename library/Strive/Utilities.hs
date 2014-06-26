{-# LANGUAGE OverloadedStrings #-}

-- | Helper functions for performing actions.
module Strive.Utilities
    ( buildRequest
    , decodeResponse
    , get
    , makeRequest
    , paginate
    , queryToSimpleQuery
    ) where

import           Data.Aeson             (FromJSON, eitherDecode)
import           Data.ByteString.Char8  (pack, unpack)
import           Data.ByteString.Lazy   (ByteString)
import           Network.HTTP.Conduit   (Request, Response, httpLbs, parseUrl,
                                         responseBody)
import           Network.HTTP.Types.URI (Query, SimpleQuery, renderSimpleQuery)
import           Strive.Client          (Client (accessToken, httpManager))
import           Strive.Types           (Page, PerPage, Resource)

-- | Build a request by constructing the URL and appending the access token.
buildRequest :: Client -> Resource -> SimpleQuery -> IO Request
buildRequest client resource query = parseUrl url
  where
    url = concat [endpoint, resource, queryString]
    endpoint = "https://www.strava.com/api/v3/"
    queryString = unpack (renderSimpleQuery True query')
    query' = ("access_token", pack (accessToken client)) : query

-- | Decode a response by parsing its body as JSON.
decodeResponse :: FromJSON a => Response ByteString -> Either String a
decodeResponse response = eitherDecode (responseBody response)

-- | Get the given resource.
get :: FromJSON a => Client -> Resource -> SimpleQuery -> IO (Either String a)
get client resource query = do
    request <- buildRequest client resource query
    response <- makeRequest client request
    return (decodeResponse response)

-- | Make an HTTP request using the client's manager.
makeRequest :: Client -> Request -> IO (Response ByteString)
makeRequest client request = httpLbs request (httpManager client)

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
