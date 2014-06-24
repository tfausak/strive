{-# LANGUAGE OverloadedStrings #-}

-- | Internal helper functions for performing actions.
module Scurry.Actions.Internal
    ( buildRequest
    , decodeResponse
    , get
    , makeRequest
    , paginate
    ) where

import           Data.Aeson             (FromJSON, decode)
import           Data.ByteString.Char8  (pack, unpack)
import           Data.ByteString.Lazy   (ByteString)
import           Network.HTTP.Conduit   (Request, Response, httpLbs, parseUrl,
                                         responseBody)
import           Network.HTTP.Types.URI (SimpleQuery, renderSimpleQuery)
import           Scurry.Client          (Client (accessToken, httpManager))

-- | Build a request by constructing the URL and appending the access token.
buildRequest :: Client -> String -> SimpleQuery -> IO Request
buildRequest client resource query = parseUrl url
  where
    url = concat [endpoint, resource, queryString]
    endpoint = "https://www.strava.com/api/v3/"
    queryString = unpack (renderSimpleQuery True query')
    query' = ("access_token", pack (accessToken client)) : query

-- | Decode a response by parsing its body as JSON.
decodeResponse :: FromJSON a => Response ByteString -> Maybe a
decodeResponse response = decode (responseBody response)

-- | Get the given resource.
get :: FromJSON a => Client -> String -> SimpleQuery -> IO (Maybe a)
get client resource query = do
    request <- buildRequest client resource query
    response <- makeRequest client request
    return (decodeResponse response)

-- | Make an HTTP request using the client's manager.
makeRequest :: Client -> Request -> IO (Response ByteString)
makeRequest client request = httpLbs request (httpManager client)

-- | Convert pagination parameters into a query.
paginate :: Integer -> Integer -> SimpleQuery
paginate page perPage =
    [ ("page", pack (show page))
    , ("per_page", pack (show perPage))
    ]
