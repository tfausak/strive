{-# LANGUAGE OverloadedStrings #-}

-- | Internal helper functions for performing actions.
module Strive.Actions.Internal
    ( buildRequest
    , decodeResponse
    , get
    , makeRequest
    , paginate
    ) where

import           Data.Aeson             (FromJSON, eitherDecode)
import           Data.ByteString.Char8  (pack, unpack)
import           Data.ByteString.Lazy   (ByteString)
import           Data.Maybe             (catMaybes)
import           Network.HTTP.Conduit   (Request, Response, httpLbs, parseUrl,
                                         responseBody)
import           Network.HTTP.Types.URI (SimpleQuery, renderSimpleQuery)
import           Strive.Client          (Client (accessToken, httpManager))
import qualified Strive.Types           as Types

-- | Build a request by constructing the URL and appending the access token.
buildRequest :: Client -> Types.Resource -> SimpleQuery -> IO Request
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
get :: FromJSON a => Client -> Types.Resource -> SimpleQuery -> IO (Either String a)
get client resource query = do
    request <- buildRequest client resource query
    response <- makeRequest client request
    return (decodeResponse response)

-- | Make an HTTP request using the client's manager.
makeRequest :: Client -> Request -> IO (Response ByteString)
makeRequest client request = httpLbs request (httpManager client)

-- | Convert pagination parameters into a query.
paginate :: Types.Page -> Types.PerPage -> SimpleQuery
paginate maybePage maybePerPage = catMaybes
    [ itemize "page" maybePage
    , itemize "per_page" maybePerPage
    ]
  where
    itemize key value = maybe Nothing (go key) value
    go key value = Just (key, pack (show value))
