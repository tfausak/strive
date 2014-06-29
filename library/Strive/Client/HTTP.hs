-- | Make HTTP calls on behalf of the client.
module Strive.Client.HTTP
    ( delete
    , get
    , post
    , put
    ) where

import           Data.Aeson            (FromJSON, eitherDecode)
import           Data.ByteString.Char8 (unpack)
import           Data.ByteString.Lazy  (ByteString)
import           Data.Monoid           ((<>))
import           Network.HTTP.Conduit  (Request, Response, checkStatus,
                                        httpLbs, method, parseUrl,
                                        responseBody)
import           Network.HTTP.Types    (Method, Query, QueryLike, methodDelete,
                                        methodGet, methodPost, methodPut,
                                        renderQuery, toQuery)
import           Strive.Client         (Client (accessToken, httpManager))
import           Strive.Types          (Resource)

-- | HTTP DELETE
delete :: (QueryLike q, FromJSON j) => Client -> Resource -> q -> IO (Either String j)
delete = http methodDelete

-- | HTTP GET
get :: (QueryLike q, FromJSON j) => Client -> Resource -> q -> IO (Either String j)
get = http methodGet

-- | HTTP POST
post :: (QueryLike q, FromJSON j) => Client -> Resource -> q -> IO (Either String j)
post = http methodPost

-- | HTTP PUT
put :: (QueryLike q, FromJSON j) => Client -> Resource -> q -> IO (Either String j)
put = http methodPost

--

http :: (QueryLike q, FromJSON j) => Method -> Client -> Resource -> q -> IO (Either String j)
http httpMethod client resource query = do
    request <- buildRequest httpMethod client resource query
    response <- performRequest client request
    return (decodeValue response)

buildRequest :: QueryLike q => Method -> Client -> Resource -> q -> IO Request
buildRequest httpMethod client resource query = do
    request <- parseUrl (buildUrl client resource query)
    return request
        { checkStatus = \ _ _ _ -> Nothing
        , method = httpMethod
        }

buildUrl :: QueryLike q => Client -> Resource -> q -> String
buildUrl client resource query = concat
    [ "https://www.strava.com/api/v3/"
    , resource
    , unpack (renderQuery True (buildQuery client <> toQuery query))
    ]

buildQuery :: Client -> Query
buildQuery client = toQuery
    [ ("access_token", accessToken client)
    ]

performRequest :: Client -> Request -> IO (Response ByteString)
performRequest client request = httpLbs request (httpManager client)

decodeValue :: FromJSON j => Response ByteString -> Either String j
decodeValue response = eitherDecode (responseBody response)
