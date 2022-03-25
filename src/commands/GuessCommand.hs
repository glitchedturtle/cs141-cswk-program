{-# LANGUAGE OverloadedStrings #-}
module GuessCommand where
import BotHandler
import Discord.Types
import Request
import ProfileCache ( profileCacheFetch, profileAddScore, profileCacheUpdate )
import MessageUtil ( emError, emSend, emGreen, formatSeconds )
import ProfileData
import Data.Char (isDigit)
import Data.Text (pack, Text, replace)
import Text.Read (readMaybe)
import Control.Monad.State
import Discord
import BotHandler (readCurrentDuration, readCurrentTime)
import ScoreUtil (calculateScoreModifier)
import Data.Maybe (fromJust)

stripMentions :: Text -> [User] -> Text
stripMentions str us = foldl (\ str u -> replace (pack u) "" str) str 
    $ concatMap formatMention us

findNumber :: String -> Maybe Int
findNumber str = readMaybe $ takeWhile isDigit $ dropWhile (not.isDigit) str

formatMention :: User -> [ String ]
formatMention user = [
    "<@" ++ id ++ ">",
    "<@!" ++ id ++ ">" ]
    where id = show $ userId user 

runGuess :: ProfileData -> Message -> BotHandler ()
runGuess profile msg = do
    currentNumber <- readCurrentNumber -- get our number from the VarT

    let guildIdText = pack $ show $ fromJust $ messageGuildId msg
    when (guildIdText `notElem` profileMemberOf profile) $ do -- check if we have recorded that the user uses this guild
        profileCacheUpdate $ profile { profileMemberOf =  guildIdText : profileMemberOf profile }

-- parse the message for the guessed number
    let guessedNumber = findNumber $ show $ stripMentions (messageContent msg) (messageMentions msg)
    case guessedNumber of
        Nothing -> lift $ void $ emSend (messageChannelId msg) $ emError "What number ??"
        Just num -> if num == currentNumber then do
            
            -- calculate how many points we are going to give
            duration <- readCurrentDuration
            let multiplier = calculateScoreModifier duration
            let score = 10 * multiplier

            -- then add those points
            profileAddScore profile score

            chooseNewNumber
            lift $ emSend (messageChannelId msg) $ def {
                createEmbedTitle  = "🥳🥳 OMG ... you guessed the number! Congratulations!!!! ",
                createEmbedDescription 
                    = "The current number has been a thing for **" <> formatSeconds duration <> "**, so you get **" 
                        <> pack (show score) <> " points** 👑 *(x" <> pack (show multiplier) <> ")*",
                createEmbedColor = Just DiscordColorGreen
            }
        else
            lift $ emSend (messageChannelId msg) $ emError $ "🙄 Nope! The number was not " 
                <> pack (show num) <> if num > 1000 then ", the number can only be between 1-1000 😜😜😜" else ""
        

run :: Request -> Message -> BotHandler ()
run (rReq, _) msg = do
    profile <- profileCacheFetch $ userId $ messageAuthor msg
    case profile of
        Nothing -> lift $ emSend (messageChannelId msg) -- make sure the user has a profile
            $ emError "😳😳 Umm.. You need to create a profile before playing the guessing game!!"
        Just prof -> runGuess prof msg
            