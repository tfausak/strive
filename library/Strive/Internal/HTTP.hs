-- | Helpers for dealing with HTTP requests.
module Strive.Internal.HTTP
  ( delete
  , get
  , post
  , put
  , buildRequest
  , performRequest
  , handleResponse
  , decodeValue
  ) where

import Data.Aeson (FromJSON, eitherDecode)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Conduit (Request, Response, checkStatus, method, parseUrl,
                             responseBody)
import Network.HTTP.Types (Method, Query, QueryLike, methodDelete, methodGet,
                           methodPost, methodPut, renderQuery, toQuery)
import Strive.Aliases (Result)
import Strive.Client (Client (client_accessToken, client_requester))

-- | Perform an HTTP DELETE request.
delete :: (QueryLike q, FromJSON j) => Client -> String -> q -> IO (Result j)
delete = http methodDelete

-- | Perform an HTTP GET request.
get :: (QueryLike q, FromJSON j) => Client -> String -> q -> IO (Result j)
get = http methodGet

-- | Perform an HTTP POST request.
post :: (QueryLike q, FromJSON j) => Client -> String -> q -> IO (Result j)
post = http methodPost

-- | Perform an HTTP PUT request.
put :: (QueryLike q, FromJSON j) => Client -> String -> q -> IO (Result j)
put = http methodPut

-- | Perform an HTTP request.
http :: (QueryLike q, FromJSON j) => Method -> Client -> String -> q -> IO (Result j)
http httpMethod client resource query = do
  request <- buildRequest httpMethod client resource query
  response <- performRequest client request
  return (handleResponse response)

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
  , unpack (renderQuery True (buildQuery client ++ toQuery query))
  ]

-- | Build a query.
buildQuery :: Client -> Query
buildQuery client = toQuery
  [ ("access_token", client_accessToken client)
  ]

-- | Actually perform an HTTP request.
performRequest :: Client -> Request -> IO (Response ByteString)
performRequest client request = (client_requester client) request

-- | Handle decoding a potentially failed response.
handleResponse :: (FromJSON j) => Response ByteString -> Result j
handleResponse response = case decodeValue response of
  Left message -> Left (response, message)
  Right value -> Right value

-- | Decode a response body as JSON.
decodeValue :: FromJSON j => Response ByteString -> Either String j
decodeValue response = eitherDecode (responseBody response)
