{-# LANGUAGE FunctionalDependencies #-}

module Strive.Lenses.Classes where

import Strive.Lenses (Lens)

class AccessTokenLens a b | a -> b where accessToken :: Lens a b
class ApprovalPromptLens a b | a -> b where approvalPrompt :: Lens a b
class AthleteLens a b | a -> b where athlete :: Lens a b
class BikesLens a b | a -> b where bikes :: Lens a b
class CityLens a b | a -> b where city :: Lens a b
class ClubsLens a b | a -> b where clubs :: Lens a b
class CountryLens a b | a -> b where country :: Lens a b
class CreatedAtLens a b | a -> b where createdAt :: Lens a b
class DatePreferenceLens a b | a -> b where datePreference :: Lens a b
class DistanceLens a b | a -> b where distance :: Lens a b
class EmailLens a b | a -> b where email :: Lens a b
class FirstnameLens a b | a -> b where firstname :: Lens a b
class FollowerCountLens a b | a -> b where followerCount :: Lens a b
class FollowerLens a b | a -> b where follower :: Lens a b
class FriendCountLens a b | a -> b where friendCount :: Lens a b
class FriendLens a b | a -> b where friend :: Lens a b
class FtpLens a b | a -> b where ftp :: Lens a b
class HttpManagerLens a b | a -> b where httpManager :: Lens a b
class IdLens a b | a -> b where id :: Lens a b
class LastnameLens a b | a -> b where lastname :: Lens a b
class MeasurementPreferenceLens a b | a -> b where measurementPreference :: Lens a b
class MutualFriendCountLens a b | a -> b where mutualFriendCount :: Lens a b
class NameLens a b | a -> b where name :: Lens a b
class PremiumLens a b | a -> b where premium :: Lens a b
class PrimaryLens a b | a -> b where primary :: Lens a b
class PrivateScopeLens a b | a -> b where privateScope :: Lens a b
class ProfileLens a b | a -> b where profile :: Lens a b
class ProfileMediumLens a b | a -> b where profileMedium :: Lens a b
class ResourceStateLens a b | a -> b where resourceState :: Lens a b
class SexLens a b | a -> b where sex :: Lens a b
class ShoesLens a b | a -> b where shoes :: Lens a b
class StateLens a b | a -> b where state :: Lens a b
class UpdatedAtLens a b | a -> b where updatedAt :: Lens a b
class WriteScopeLens a b | a -> b where writeScope :: Lens a b
