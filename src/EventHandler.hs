{-# LANGUAGE OverloadedStrings #-}
module EventHandler where

import MessageUtil ( emError, em, isFromBot, mentions )
import Request
    ( RequestScope(..),
      RequestCommand(Play, CreateProfile, ShowProfile, ShowLeaderboard),
      Request,
      fromWobbly,
      reqCommand,
      RequestWobbly )
import BotHandler (BotHandler)

import qualified CreateProfileCommand
import qualified ShowProfileCommand
import qualified GuessCommand
import qualified ShowLeaderboardCommand

import Data.Text (Text, pack, toLower, isInfixOf)
import Data.List (find)
import Data.Maybe (isNothing, isJust, fromJust)
import Control.Monad (when, void, unless)
import Control.Monad.State (lift)

import Discord ( readCache, restCall, cacheCurrentUser )
import Discord.Types
    ( Message(messageContent, messageMentions, messageGuildId,
              messageChannelId),
      User(userIsBot, userId) )
import qualified Discord.Requests as R

commandMap :: [ (Text, RequestCommand) ]
commandMap = [
    ("create", CreateProfile),
    ("init", CreateProfile),
    ("profile", ShowProfile),
    ("leaderboard", ShowLeaderboard),
    ("guess", Play),
    ("play", Play) ]

-- map every command to a function that will execute the command
runCommand :: RequestCommand -> Request -> Message -> BotHandler ()
runCommand CreateProfile = CreateProfileCommand.run
runCommand ShowProfile = ShowProfileCommand.run
runCommand ShowLeaderboard = ShowLeaderboardCommand.run
runCommand Play = GuessCommand.run
runCommand _ = error "Running command with no registered handler!"

parseRequest :: Message -> RequestWobbly
parseRequest msg = (search commandMap content, parseScope msg)
    where content = (toLower.messageContent) msg

-- the scope is a commonly used parameter for commands that describes what the bot should compare
-- it can compare an individual user, or the entire server, or the entire world.
-- this was more useful with the original intentions of the bot, such as a graphing command (graph yourself vs the world average, or just the average of yourself etc)
parseScope :: Message -> Maybe RequestScope
parseScope msg
  | length mentions == 2 = case find (not.userIsBot) mentions of -- if we mention another user, we want user scope
      Nothing -> Nothing
      Just user -> Just $ Profile (userId user)
  | isJust guildId && "server" `isInfixOf` content = Server <$> guildId -- if we are in a guild, and we include the phrase "server"
  | "world" `isInfixOf` content = Just Global -- if we include the word "world", we probably want global
  | otherwise = Nothing
  where
      content = toLower $ messageContent msg -- toLower the content, so we can catch all capitalisations
      guildId = messageGuildId msg
      mentions = messageMentions msg

-- find a matching string, and then return its respective object. used to find what command to execute
search :: [(Text, a)] -> Text -> Maybe a
search assocArr toSearch = snd <$> find (\(k,v) -> k `isInfixOf` toSearch) assocArr

-- validate a request before we attempt to execute it
-- most important one is that we actually make sure there is a command to execute
isRequestValid :: Message -> RequestWobbly -> Maybe Text
isRequestValid msg (rCmd, rScope)
    | isNothing $ messageGuildId msg = Just "😿 Sorry! You can only execute commands from within a guild!"
    | isFromBot msg = Just "Bots can't execute commands! You silly little robot 🤣🤣🤣"
    | isNothing rCmd = Just "You silly goose! !! You didn't provide a command to run 🤪 or you probably spelt it wrong !!! haha rookie mistake i guess"
    | otherwise = Nothing

handleMessage :: Message -> BotHandler ()
handleMessage msg = unless (isFromBot msg) $ do
    user <- lift $ cacheCurrentUser <$> readCache
    when(mentions msg user) $ do -- if we are mentioned,
        case isRequestValid msg request of -- check if the request is valid
            Nothing -> runCommand (reqCommand request') request' msg -- Nothing means no errors, so we can go ahead and execute
            Just errorMsg -> lift $ void $ restCall $ em (messageChannelId msg) $ emError errorMsg -- output any validation fails back to the user
        where
            request = parseRequest msg
            request' = fromWobbly request -- we know there is a valid command, so we can use a form of request that doesn't have Maybe RequestCommand
