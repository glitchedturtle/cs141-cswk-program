{-# LANGUAGE OverloadedStrings #-}
module ProfileData where
import Database.MongoDB
import Data.Text (Text, empty)
import Data.Int (Int64)
import Request (RequestScope, scopeSerialize)
import Discord.Internal.Types (GuildId)

-- contains the definitions for profile data and leaderboard data

-- serializable can be converted to and from a mongo document
class Serializable a where
    serialize :: a -> Document
    deserialize :: Document -> a

data ProfileData = ProfileData {
    profileUserId :: Text,
    profileName :: Text,
    profileScore :: Float,
    profileScoreHistory :: [ (Int, Float) ],
    profileMemberOf :: [ Text ]
} deriving (Show, Eq)

data LeaderboardEntry = LeaderboardEntry {
    lbEntryUserId :: Text,
    lbEntryName :: Text,
    lbEntryScore :: Float,
    lbEntryModified :: Int
} deriving(Show, Eq)

data LeaderboardData = LeaderboardData {
    _lbCreated    :: Text, -- used for debugging, is this a new leaderboard or from mongo
    leaderboardId :: Text,
    leaderboardEntries :: [LeaderboardEntry]
} deriving (Show, Eq)

instance Serializable ProfileData where
    serialize a = [
        "_id"          =: profileUserId a,
        "name"         =: profileName a,
        "score"        =: profileScore a,
        "scoreHistory" =: map (\(a,b)->[ show a, show b ]) (profileScoreHistory a) , -- mongo cant serialize tuples so we do it for them
        "memberOf"     =: profileMemberOf a ]
    deserialize a = ProfileData {
        profileUserId       = at "_id" a,
        profileName         = at "name" a,
        profileScore        = at "score" a,
        profileScoreHistory = map (\x->(read $ head x, read $ head $ tail x)) $ at "scoreHistory" a, -- as above
        profileMemberOf     = at "memberOf" a }

instance Serializable LeaderboardEntry where
    serialize a = [
        "_id"      =: lbEntryUserId a,
        "name"     =: lbEntryName a,
        "score"    =: lbEntryScore a,
        "modified" =: lbEntryModified a ]
    deserialize a = LeaderboardEntry {
        lbEntryUserId   = at "_id" a,
        lbEntryName     = at "name" a,
        lbEntryScore    = at "score" a,
        lbEntryModified = at "modified" a }

instance Serializable LeaderboardData where
    serialize a = [
        "_id"   =: leaderboardId a,
        "entries" =: map serialize (leaderboardEntries a) ] -- entries in the leaderboard are documents, serialize them
    deserialize a = LeaderboardData {
        _lbCreated      = "deserialized",
        leaderboardId   = at "_id" a,
        leaderboardEntries = map deserialize $ at "entries" a } -- as above

-- instantiate a new empty leaderboard
emptyLeaderboard :: RequestScope -> LeaderboardData
emptyLeaderboard x = LeaderboardData {
    _lbCreated = "emptyLeaderboard",
    leaderboardId = scopeSerialize x,
    leaderboardEntries = [] }