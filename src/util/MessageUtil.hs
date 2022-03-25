module MessageUtil where

import Discord
import Discord.Types
import qualified Discord.Requests as R

import Data.Text (Text, pack)
import Data.List (find)
import Data.Maybe (isJust)
import Control.Monad (void)

-- check if a message is from a bot
isFromBot :: Message -> Bool
isFromBot = userIsBot.messageAuthor

-- check if a message mentions a specific user, used to check if a user mentions us
mentions :: Message -> User -> Bool
mentions msg user = isJust $ find (\x -> userId x == userId user) (messageMentions msg)

-- Util function that helps create aembed objects
em :: ChannelId -> CreateEmbed -> R.ChannelRequest Message
em id embed = R.CreateMessageDetailed id $ def {
        R.messageDetailedEmbeds = Just [ embed ]
    }

-- similar to 'em', but also sends the embed to a channel
emSend :: ChannelId -> CreateEmbed -> DiscordHandler ()
emSend id embed = void $ restCall $ em id embed

-- Create an embed in a specific format for errors
emError :: Text -> CreateEmbed
emError msg = def {
        --createEmbedTitle = "Oops!",
        createEmbedDescription = msg,
        createEmbedColor = Just DiscordColorRed
    }

-- similar to above, but for non-error messages
emGreen :: Text -> CreateEmbed
emGreen msg = def {
        createEmbedDescription  = msg,
        createEmbedColor = Just DiscordColorGreen
    }


timeFormats = [
    (1, " seconds"),
    (60, " minutes"),
    (60 * 60, " hours"),
    (60 * 60 * 24, " days"),
    (60 * 60 * 24 * 30, " months"),
    (60 * 60 * 24 * 30 * 12, " years"),
    (10 ** 30, "globs" ) ]

-- format a float to 1 dp
formatFloat :: Float -> Text
formatFloat float = pack $ show $ fromIntegral (round (10 * float)) / 10

-- inputs a number of seconds and outputs that in a reasonable time (for example: 120s = 2 mins)
formatTime :: Float -> Text
formatTime time = formatFloat (time/fst format) <> pack (snd format)
    where format = head $ until ((time <).fst.head.tail) tail timeFormats

-- wrapper so we dont have to convert to float every time
formatSeconds :: Int -> Text
formatSeconds = formatTime.fromIntegral