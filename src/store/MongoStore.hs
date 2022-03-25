{-# LANGUAGE OverloadedStrings #-}
module MongoStore where

import Discord.Types (UserId)
import Database.MongoDB
import qualified Database.MongoDB.Transport.Tls as DBTLS
import Control.Monad (void)
import Control.Exception (catch, Exception, try, SomeException (SomeException))
import Data.Maybe (isJust)
import Data.Text (Text, pack)
import BotHandler ( readMongo, writeMongo, BotHandler )
import Control.Monad.State (liftIO)
import Control.Monad.Reader

import ProfileData
import Request (RequestScope, scopeSerialize)
import GHC.IO (catchAny)
import Control.Concurrent (forkIO)
import BotConfig (cfMongoDoAuth, cfMongoUsername, cfMongoPassword, cfMongoDb, cfMongoHost, cfMongoPort, cfMongoUseTLS)
import GHC.IO.Exception (IOException(IOError))

mong :: Action IO a -> BotHandler a
mong action = do
    existingPipe <- readMongo
    pipe <- case existingPipe of
        Nothing -> do
            newCon <- liftIO mongoMakeConnection
            writeMongo newCon

            return newCon
        Just con -> return con

    res <- liftIO $ try $ access pipe master cfMongoDb action
    case res of
        Left e -> do -- if theres an error, we want to output it
            let e' = e :: SomeException
            liftIO $ putStrLn ("[Mongo] Error while executing: " ++ show e) -- output the error

            error "Error"
        Right ret -> return ret

mongoMakeConnection :: IO Pipe
mongoMakeConnection = do
    putStrLn "[Mongo] Attempting connection to mongo server..."
    dbRes <- try $ if cfMongoUseTLS then 
        DBTLS.connect cfMongoHost cfMongoPort
        else connect $ Host cfMongoHost cfMongoPort
    case dbRes of
        Left e -> do -- if theres an error while connecting
            let e' = e :: SomeException
            liftIO $ putStrLn ("[Mongo] Error while connecting: " ++ show e) -- output the error

            error "Error connecting..."
        Right pipe -> do
            putStrLn "[Mongo] Connection established.."
            when cfMongoDoAuth $ do -- if we need to authenticate, do it
                putStrLn "[Mongo] Attempting authentication..."
                void $ access pipe UnconfirmedWrites "admin" (auth cfMongoUsername cfMongoPassword)
                putStrLn "[Mongo] Authenticated"
            return pipe

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