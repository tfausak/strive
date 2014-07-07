-- | Helpers for dealing with HTTP requests.
module Strive.Internal.HTTP where

import Data.Aeson (FromJSON, eitherDecode)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy (ByteString)
import Data.Monoid ((<>))
import Network.HTTP.Conduit (Request, Response, checkStatus, httpLbs, method,
                             parseUrl, responseBody)
import Network.HTTP.Types (Method, Query, QueryLike, methodDelete, methodGet,
                           methodPost, methodPut, renderQuery, toQuery)
import Strive.Client (Client (client_accessToken, client_httpManager))

-- | Perform an HTTP DELETE request.
delete :: (QueryLike q, FromJSON j) => Client -> String -> q -> IO (Either String j)
delete = http methodDelete

-- | Perform an HTTP GET request.
get :: (QueryLike q, FromJSON j) => Client -> String -> q -> IO (Either String j)
get = http methodGet

-- | Perform an HTTP POST request.
post :: (QueryLike q, FromJSON j) => Client -> String -> q -> IO (Either String j)
post = http methodPost

-- | Perform an HTTP PUT request.
put :: (QueryLike q, FromJSON j) => Client -> String -> q -> IO (Either String j)
put = http methodPut

-- | Perform an HTTP request.
http :: (QueryLike q, FromJSON j) => Method -> Client -> String -> q -> IO (Either String j)
http httpMethod client resource query = do
  request <- buildRequest httpMethod client resource query
  response <- performRequest client request
  return (decodeValue response)

-- | Build a request.
buildRequest :: QueryLike q => Method -> Client -> String -> q -> IO Request
buildRequest httpMethod client resource query = do
  request <- parseUrl (buildUrl client resource query)
  return request
    { checkStatus = \ _ _ _ -> Nothing
    , method = httpMethod
    }

-- | Build a URL.
buildUrl :: QueryLike q => Client -> String -> q -> String
buildUrl client resource query = concat
  [ "https://www.strava.com/"
  , resource
  , unpack (renderQuery True (buildQuery client <> toQuery query))
  ]

-- | Build a query.
buildQuery :: Client -> Query
buildQuery client = toQuery
  [ ("access_token", client_accessToken client)
  ]

-- | Actually perform an HTTP request.
performRequest :: Client -> Request -> IO (Response ByteString)
performRequest client request = httpLbs request (client_httpManager client)

-- | Decode a response body as JSON.
decodeValue :: FromJSON j => Response ByteString -> Either String j
decodeValue response = eitherDecode (responseBody response)
