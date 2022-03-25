module BotHandler where

import Request (RequestScope, scopeSerialize)
import ProfileData (ProfileData, LeaderboardEntry, LeaderboardData, leaderboardId)

import Database.MongoDB (MongoContext, Action, Pipe)
import GHC.Conc (TVar, readTVarIO, writeTVar, atomically)
import Control.Monad.Reader (ReaderT, MonadReader (ask))
import Control.Monad.State (liftIO)
import System.Random (randomRIO)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Text (Text)
import qualified Data.Map as Map

import Discord.Internal.Rest (Request)
import Discord (DiscordHandler)

data BotHandle = BotHandle {

    botMongoCon :: TVar (Maybe Pipe), -- the mongo connection object

    botCurrentNumber :: TVar Int, -- the current number that people will try to guess
    botNumberSince :: TVar Int, -- the time when the number was chosen (in seconds since epoch)

    botLeaderboardMap :: TVar (Map.Map Text LeaderboardData) -- the leaderboard cache

}

-- define the monad t stack we the project uses
type BotHandler = ReaderT BotHandle DiscordHandler

chooseNewNumber :: BotHandler ()
chooseNewNumber = do
    randomNumber <- liftIO $ randomRIO (1,1000)
    currentTime <- liftIO $ fromIntegral.round <$> getPOSIXTime
    botState <- readBotState

    liftIO $ atomically $ writeTVar (botCurrentNumber botState) randomNumber
    liftIO $ atomically $ writeTVar (botNumberSince botState) currentTime
    liftIO $ putStrLn $ "A new random number has been selected! " ++ show randomNumber

-- read the bot state from the BotHandler monad
readBotState :: BotHandler BotHandle
readBotState = ask

-- util functions to read and write to the TVars
readB :: (BotHandle -> TVar a) -> BotHandler a
readB m = do
    tVar <- m <$> readBotState
    liftIO $ readTVarIO tVar

readCurrentNumber :: BotHandler Int
readCurrentNumber = readB botCurrentNumber

readCurrentTime :: BotHandler Int 
readCurrentTime = liftIO $ round <$> getPOSIXTime

readNumberSince :: BotHandler Int
readNumberSince = readB botNumberSince

readMongo :: BotHandler (Maybe Pipe)
readMongo = readB botMongoCon

readLeaderboard :: RequestScope -> BotHandler (Maybe LeaderboardData)
readLeaderboard requestScope = do
    map <- readB botLeaderboardMap
    return $ Map.lookup (scopeSerialize requestScope) map

readCurrentDuration :: BotHandler Int
readCurrentDuration = do
    timeSince <- readNumberSince
    currentTime <- readCurrentTime
    return (currentTime - timeSince)

writeMongo :: Pipe -> BotHandler ()
writeMongo pipe = do
    tVar <- botMongoCon <$> readBotState
    liftIO $ atomically $ writeTVar tVar (Just pipe)

writeLeaderboard :: LeaderboardData -> BotHandler ()
writeLeaderboard lbData = do
    tVar <- botLeaderboardMap <$> readBotState
    map <- liftIO $ readTVarIO tVar
    liftIO $ atomically $ writeTVar tVar $ Map.insert (leaderboardId lbData) lbData map