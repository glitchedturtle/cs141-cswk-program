{-# LANGUAGE OverloadedStrings #-}
module CreateProfileCommand where
import Discord
import Discord.Types
import Request
import Control.Monad (void)
import ProfileCache (profileCacheFetch, profileCacheCreate)
import Control.Monad.State (liftIO, MonadTrans (lift))
import Database.MongoDB (Action)
import BotHandler
import MessageUtil
import MongoStore
import Data.Text (pack)
import Data.Maybe

import ProfileData
import Data.Time
import Data.Time.Clock.POSIX
import ProfileData (ProfileData(profileMemberOf))

run :: Request -> Message -> BotHandler ()
run _ msg = do
    hasProfile <- profileCacheFetch (userId $ messageAuthor msg)
    if isJust hasProfile then -- check if we already have a profile (so they can't have two!)
        lift $ void $ restCall $ em (messageChannelId msg) $ emError "You already have a profile! You little goober hehehe"
    else do

        epoch <- readCurrentTime
        let profileData = ProfileData { -- create an initial profile data object to add
            profileUserId       = pack.show $ userId $ messageAuthor msg,
            profileName         = userName $ messageAuthor msg,
            profileScore        = 0,
            profileScoreHistory = [ (epoch, 0) ],
            profileMemberOf     = [ pack $ show $ fromJust $ messageGuildId msg ]
        }

        err <- profileCacheCreate profileData -- send request to store
        lift $ void $ emSend (messageChannelId msg) $ def {
            createEmbedTitle  = "Congratulations on making your profile!!",
            createEmbedDescription = "I can't belive you've done it!!!! You're the best! maybe try the **profile** command now ?? but you do you",
            createEmbedColor = Just DiscordColorGreen
        }