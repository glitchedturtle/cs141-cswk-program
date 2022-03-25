{-# LANGUAGE OverloadedStrings #-}
module ShowProfileCommand where

import BotHandler (BotHandler)
import Discord
import Discord.Types
import Request
import MessageUtil (emError, emGreen, formatFloat, emSend, em)
import Control.Monad.Reader (lift)
import ProfileCache
import MongoStore
import ProfileData
import Data.Text (Text, pack)
import Data.List (findIndex)
import Data.Maybe (fromJust)

-- convert two places (ie 3rd in world, 2nd in server etc) to a little tag that we can put in the embed
formatPlace :: Maybe Int -> Maybe Int -> Text 
formatPlace (Just a) (Just b) = "*(#" <> pack (show a) <> " world, #" <> pack (show b) <> " server)*"
formatPlace (Just a) _        = "*(#" <> pack (show a) <> " world)*" 
formatPlace _        (Just b) = "*(#" <> pack (show b) <> " server)*" -- pretty sure this case is impossible, but just in case 
formatPlace _        _        = ""

runOn :: Message -> UserId -> BotHandler ()
runOn msg userId = do
    profile <- profileCacheFetch userId

    case profile of
        Nothing -> lift $ emSend (messageChannelId msg) $ emError "😳😳 Umm.. I didn't find a profile there... Sorry?"
        Just profile -> do

            -- check their places on any relevant leaderboard
            lbs <- mapM leaderboardFetch $ getLeaderboardScopes profile
            let places = map (\x -> (x, findIndex (\x -> lbEntryUserId x == profileUserId profile) $ leaderboardEntries x))

            placeInGlobal <- fmap (1+) . findIndex ((profileUserId profile ==).lbEntryUserId) . leaderboardEntries 
                <$> leaderboardFetch Global
            placeInServer <- fmap (1+) . findIndex ((profileUserId profile ==).lbEntryUserId) . leaderboardEntries 
                <$> leaderboardFetch (Server (fromJust $ messageGuildId msg))
            
            lift $ emSend (messageChannelId msg) $ def {
                createEmbedTitle = "📖 **" <> profileName profile <> "'s profile** " 
                    <> (if placeInGlobal == Just 1 then "👑" else "")
                    <> (if placeInServer == Just 1 then "🏅" else ""), -- if they are first on a leaderboard, we give them a little icon
                createEmbedDescription = "**Score: **" <> formatFloat (profileScore profile) <> " " 
                    <> formatPlace placeInGlobal placeInServer,
                createEmbedColor = Just DiscordColorBlue
            }


run :: Request -> Message -> BotHandler ()
run (rReq, rScope) msg = case rScope of
    Nothing -> runOn msg $ userId $ messageAuthor msg -- default to running it on sender
    Just (Profile id) -> runOn msg id
    _ -> lift $ emSend (messageChannelId msg)
        $ emError "not enough info you goofy guy 😠 maybe next time tell me who to query .. and don't make me ask again!!!"
    