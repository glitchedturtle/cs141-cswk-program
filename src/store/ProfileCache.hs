module ProfileCache where

import ProfileData
import Request (RequestScope (Profile, Server), scopeSerialize, RequestScope(Global, Server) )
import BotHandler (BotHandler, BotHandle, readCurrentTime, readLeaderboard, writeLeaderboard)
import BotConfig (cfLeaderboardSize)
import MongoStore ( mongoFetchProfile, mongoCreateProfile, mongoUpdateProfile, mongoFetchLeaderboard, mongoUpdateLeaderboard )

import Database.MongoDB (Action)
import Control.Monad.State (lift, MonadIO (liftIO))
import Control.Monad (mapM)
import Data.Maybe (fromMaybe)
import Data.Text (pack, unpack)
import Data.List ((\\), deleteBy)

import Discord (DiscordHandler)
import Discord.Types

-- abstractions of mongo db operations so that in the future it is easy to cache profiles
profileCacheFetch :: UserId -> BotHandler (Maybe ProfileData)
profileCacheFetch = mongoFetchProfile

profileCacheCreate :: ProfileData -> BotHandler ()
profileCacheCreate = mongoCreateProfile

profileCacheUpdate :: ProfileData -> BotHandler ()
profileCacheUpdate = mongoUpdateProfile

-- add score to a profile
profileAddScore :: ProfileData -> Float -> BotHandler ()
profileAddScore profile score = do
    time <- readCurrentTime

    -- create a copy with the new score, as well as an updated history
    let newProfile = profile {
        profileScore= newScore,
        profileScoreHistory  = take 100 $ (time, newScore) : profileScoreHistory profile } -- we only keep the 100 most recent data points

    -- push the changes to the datastore, and update leaderboards if required
    profileCacheUpdate newProfile
    leaderboardUpdate newProfile

    where newScore = profileScore profile + score

-- get every possible leaderboard the profile could be on
getLeaderboardScopes :: ProfileData -> [RequestScope]
getLeaderboardScopes pd = Global : map (\x -> Server( read $ unpack x )) (profileMemberOf pd)

-- add a leaderboard entry to the leaderboard in the right place (including the end)
insertInto :: LeaderboardEntry -> [LeaderboardEntry] -> [LeaderboardEntry]
insertInto entry entries =
   takeWhile ((lbEntryScore entry <=).lbEntryScore) entries
    ++ [ entry ]
    ++ dropWhile ((lbEntryScore entry <=).lbEntryScore) entries

-- update a specific leaderboard
leaderboardUpdate' :: Int -> ProfileData -> LeaderboardData -> LeaderboardData
leaderboardUpdate' time pd lbd = lbd {
        leaderboardEntries = take cfLeaderboardSize $ insertInto entry -- add ourselves back in the right place, and trunctate the leaderboard to its size
            $ deleteBy (\x y -> lbEntryUserId x == lbEntryUserId y) entry -- remove ourselves if we are there, so we dont get duplicate entries
            $ leaderboardEntries lbd } -- fetch the current entries
    where
        entry = LeaderboardEntry { lbEntryUserId = profileUserId pd, lbEntryName = profileName pd, lbEntryScore = profileScore pd, lbEntryModified = time}

-- update leaderboards based on a new profile data
leaderboardUpdate :: ProfileData -> BotHandler ()
leaderboardUpdate pd = do
    curTime <- readCurrentTime
    leaderboards <- mapM leaderboardFetch $ getLeaderboardScopes pd -- fetch all the leaderboards we could be a part of
    let updated = map (leaderboardUpdate' curTime pd) leaderboards

    mapM_ leaderboardWrite $ updated \\ leaderboards -- remove what hasnt changed


leaderboardWrite :: LeaderboardData -> BotHandler ()
leaderboardWrite lbData = do
    writeLeaderboard lbData
    mongoUpdateLeaderboard lbData

-- simple lazy-loading cache pattern 
leaderboardFetch :: RequestScope -> BotHandler LeaderboardData
leaderboardFetch (Profile _) = error "Attempting to fetch profile leaderboard (invalid)"
leaderboardFetch x = do
    cached <- readLeaderboard x -- read the cache
    case cached of
        Just lb -> return lb -- if we are there, return the cached value
        Nothing -> do -- otherwise load the cached value from mongo
            leaderboard <- fromMaybe (emptyLeaderboard x) <$> mongoFetchLeaderboard x
            writeLeaderboard leaderboard -- and add it to the cache
            return leaderboard