-- | <http://strava.github.io/api/v3/clubs/#detailed-representation-attributes-a-iddetailednbspa>
module Scurry.Objects.Clubs.ClubDetailed
    ( ClubDetailed (..)
    ) where

import           Data.Text (Text)

-- | Detailed club representation.
data ClubDetailed = ClubDetailed
    { city          :: Text
    , club_type     :: Text
    , country       :: Text
    , description   :: Text
    , id            :: Integer
    , memberCount   :: Integer
    , name          :: Text
    , private       :: Bool
    , profile       :: Text
    , profileMedium :: Text
    , resourceState :: Integer
    , sportType     :: Text
    , state         :: Text
    } deriving (Show)
