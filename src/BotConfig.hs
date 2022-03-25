{-# LANGUAGE OverloadedStrings #-}
module BotConfig where

import Data.Text (Text)
import Database.MongoDB (Database, Username, Password, PortID (PortNumber))

cfDiscordToken :: Text
cfLeaderboardSize :: Int
cfMongoHost :: String
cfMongoPort :: PortID
cfMongoDb :: Database 
cfMongoDoAuth :: Bool
cfMongoUseTLS :: Bool
cfMongoUsername :: Username 
cfMongoPassword :: Password 

cfDiscordToken = "OTU0NDMxOTg0MTM3MDM1OTA2.YjTCDQ.RwhUfmQnjm6qQCrmSidJvdObQps"
cfLeaderboardSize = 10

-- the mongo library is terrible
-- to connect to atlas you have to jump through a bunch of hoops

-- use this for pre-created atlas instance (feel free to connect yourself and modify stuff)
cfMongoHost = "countbot-shard-00-01.6czpo.mongodb.net"
cfMongoPort = PortNumber 27017
cfMongoDb   = "countbot"
cfMongoDoAuth = True
cfMongoUseTLS = True
cfMongoUsername = "auth"
cfMongoPassword = "m7qRzrffAmuF3FR"

-- use this for a local instance of mongo
{-cfMongoHost = "127.0.0.1"
cfMongoPort = PortNumber 27017
cfMongoDb   = "countbot"
cfMongoDoAuth = False
cfMongoUseTLS = False
cfMongoUsername = ""
cfMongoPassword = ""-}
