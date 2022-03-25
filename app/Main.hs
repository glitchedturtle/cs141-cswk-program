{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified EventHandler
import MongoStore
import BotHandler (BotHandler, BotHandle( BotHandle, botCurrentNumber, botNumberSince, botLeaderboardMap, botMongoCon) )
import Control.Monad.Reader ( ReaderT, liftIO, runReaderT )
import BotConfig (cfDiscordToken)

import Control.Monad.IO.Class (liftIO)
import Discord
import Discord.Types

import System.Random
import qualified Data.Text.IO as TIO
import GHC.Conc (newTVarIO)
import Data.Text (Text, pack)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Random (randomRIO)

-- simple function to extract the user name and discrim (like the format used by discord) from a user object
formatName :: User -> Text
formatName user = userName user <> "#" <> fromMaybe "XXXX" (userDiscrim user)

-- our start handler, used pretty much to set the bot's activity (and check the bot is working)
handleStart :: DiscordHandler()
handleStart = do
    sendCommand $ UpdateStatus UpdateStatusOpts {
        updateStatusOptsSince = Nothing,
        updateStatusOptsNewStatus = UpdateStatusDoNotDisturb,
        updateStatusOptsAFK = False,
        updateStatusOptsGame = Just $ def {
            activityName = "cs 141 monad lecture",
            activityType = ActivityTypeWatching
        }
    }
    cachedUser <- cacheCurrentUser <$> readCache
    liftIO $ TIO.putStrLn ("Started bot! Authorized as " <> formatName cachedUser)

-- the handler function routes different types of events to different handlers - at the moment, it only forwards messages
-- and drops everything else, but we are open to extension
handler :: Event -> BotHandler ()
handler (MessageCreate m) = EventHandler.handleMessage m
handler _ = return ()

main :: IO ()
main = do

    initialNum <- randomRIO (1, 1000)
    time <- round <$> getPOSIXTime
    TIO.putStrLn $ "The initial number chosen is " <> pack (show initialNum)

    -- initialise the TVars and add them to the BotHandle wrapper
    numberVar <- newTVarIO initialNum
    numberSinceVar <- newTVarIO time
    leaderboardCacheVar <- newTVarIO Map.empty
    mongoVar <- newTVarIO Nothing
    let botHandle = BotHandle {
        botCurrentNumber = numberVar,
        botNumberSince = numberSinceVar,
        botLeaderboardMap = leaderboardCacheVar,
        botMongoCon = mongoVar
    }

    -- and then start the bot
    err <- runDiscord $ def {
        discordToken = cfDiscordToken,
        discordOnStart = handleStart,
        discordOnEvent = \x -> runReaderT (handler x) botHandle
    }

    TIO.putStrLn err
