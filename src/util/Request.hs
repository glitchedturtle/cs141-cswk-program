{-# LANGUAGE OverloadedStrings #-}
module Request where

import Discord.Types ( GuildId, UserId )
import Data.Text (Text, pack)
    
data RequestCommand = CreateProfile | ShowProfile | ShowLeaderboard | ShowGraph | Play
data RequestScope = Profile UserId | Server GuildId | Global

type RequestWobbly = (Maybe RequestCommand, Maybe RequestScope)
type Request = (RequestCommand, Maybe RequestScope)

fromWobbly :: RequestWobbly -> Request
fromWobbly (Just x, z) = (x, z)
fromWobbly _ = error "Failed to convert wobbly command to command"

scopeSerialize :: RequestScope -> Text
scopeSerialize Global = "global"
scopeSerialize (Server snowflake) = "server_" <> pack (show snowflake)
scopeSerialize (Profile userId)   = "user_" <> pack (show userId)

reqCommand :: Request -> RequestCommand
reqCommand (x, _) = x