{-# LANGUAGE OverloadedStrings #-}
module MongoStore where

import Discord.Types (UserId)
import Database.MongoDB
import Control.Monad (void)
import Control.Exception (catch, Exception)
import Data.Maybe (isJust)
import Data.Text (Text, pack)
import BotHandler ( readMongo, writeMongo, BotHandler )
import Control.Monad.State (liftIO)
import Control.Monad.Reader

import ProfileData
import Request (RequestScope, scopeSerialize)
import GHC.IO (catchAny)
import Control.Concurrent (forkIO)
import BotConfig (cfMongoHost)

mong :: Action IO a -> BotHandler a
mong action = do
    existingPipe <- readMongo
    pipe <- case existingPipe of
        Nothing -> do
            newCon <- liftIO mongoMakeConnection
            writeMongo newCon
            return newCon
        Just con -> return con

    liftIO $ access pipe master "statbot" action

mongoMakeConnection :: IO Pipe
mongoMakeConnection = connect (host cfMongoHost)

mongoFetchProfile :: UserId -> BotHandler (Maybe ProfileData)
mongoFetchProfile userId = fmap deserialize <$>
    mong (findOne (select [ "_id" =: show userId ] "profile"))

mongoCreateProfile :: ProfileData -> BotHandler ()
mongoCreateProfile profileData = void $ mong $ insert "profile" (serialize profileData)

mongoUpdateProfile :: ProfileData -> BotHandler ()
mongoUpdateProfile profileData = void $ mong $ save "profile" $ serialize profileData

mongoFetchLeaderboard :: RequestScope -> BotHandler (Maybe LeaderboardData)
mongoFetchLeaderboard scope = fmap deserialize <$>
    mong (findOne (select [ "_id" =: scopeSerialize scope ] "leaderboard"))

mongoUpdateLeaderboard :: LeaderboardData -> BotHandler ()
mongoUpdateLeaderboard lbData = void $ mong $ save "leaderboard" $ serialize lbData