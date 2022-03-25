{-# LANGUAGE OverloadedStrings #-}
module ShowLeaderboardCommand where

import Discord.Types
import Request (Request, RequestScope (Profile, Global, Server))
import BotHandler (BotHandler, readCurrentTime)
import ProfileCache (leaderboardFetch)
import MessageUtil (emSend, formatFloat, formatTime)
import Discord (def, readCache, Cache (cacheGuilds))
import Data.Text (Text, pack, unpack)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Control.Monad.State (lift, MonadIO (liftIO))
import Control.Monad (void)
import ProfileData (leaderboardEntries, LeaderboardEntry (lbEntryUserId, lbEntryScore, lbEntryModified), LeaderboardData (leaderboardEntries, leaderboardId, _lbCreated), lbEntryName)

getGuild :: GuildId -> BotHandler (Maybe Guild)
getGuild id = lift $ Map.lookup id . cacheGuilds <$> readCache

-- converts guild member count to appropriate emoji
titleEmoji :: Integer -> Text
titleEmoji x
    | x < 0  = "👽" -- erroneous case
    | x < 10 = "🥱"
    | x < 50 = "😴"
    | x < 100 = "😐"
    | x > 1000 = "😱"
    | otherwise = "😺"

-- title used for the leaderboard embed
leaderboardTitle :: RequestScope -> BotHandler Text
leaderboardTitle Global = return "__**Leaderboard of the world!!!**__ 😱😱"
leaderboardTitle (Server id) = do
    guild <- getGuild id
    let gName = maybe "idk" guildName guild
    let gSize = maybe (Just (-1)) guildMemberCount guild
    return $ "**Leaderboard of " <> gName <> " ** " <> titleEmoji (fromMaybe (-1) gSize)
leaderboardTitle _ = return "👽👽👽"

runScope :: RequestScope -> Message -> BotHandler ()
runScope scope msg = do
    leaderboard <- leaderboardFetch scope
    title <- leaderboardTitle scope
    curTime <- readCurrentTime

    -- convert each leaderboard entry to an embed field
    let entries = zipWith (\i en -> EmbedField
            ("**"<> pack (show i) <> ". " <> lbEntryName en <> "** - " <> formatFloat (lbEntryScore en) <> " points") -- top
            ("*Last updated " <> formatTime (fromIntegral (curTime - lbEntryModified en)) <> " ago*") -- bottom
            (Just False)) [ 1.. ] $ leaderboardEntries leaderboard

    lift $ void $ emSend (messageChannelId msg) $ def {
        createEmbedTitle = title,
        createEmbedDescription = "The current top scorers in this category are...",
        createEmbedColor = Just DiscordColorDiscordFuschia,
        createEmbedFields = if not $ null entries then entries
            else [ EmbedField "😐 **There doesn't seem to be anything here...**" "*hi i couldnt figure out invisible characters so this text has to be here*" Nothing ]
    }

run :: Request -> Message -> BotHandler ()
run (_, rScope) = case rScope of -- make sure we have a scope, if not then default to global
    Nothing -> runScope Global
    Just (Profile _) -> runScope Global
    Just x -> runScope x