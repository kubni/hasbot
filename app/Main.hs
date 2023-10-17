{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad (when, void)
import           Control.Monad.IO.Class (liftIO)
import           UnliftIO.Concurrent
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import           Discord
import           Discord.Types
import           Discord.Internal.Types.Embed
import qualified Discord.Requests as R


import HasbotCommands

-- Testing
import Data.List

main :: IO ()
main = do
  botToken <- TIO.readFile "most_secret_token_ever"
  userFacingError <- runDiscord $ def
           { discordToken = botToken
           , discordOnEvent = eventHandler
           , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
           }
  TIO.putStrLn userFacingError

eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
        MessageCreate m -> when (notFromBot m && isCommand m) $ do
          botResponse <- liftIO $ parseCommand m
          liftIO $ print botResponse
          let opts :: R.MessageDetailedOpts
              opts =
                def {
                      R.messageDetailedEmbeds = Just [ def {
                                                              createEmbedDescription = T.pack botResponse
                                                           }
                                                     ]
                    }
          void $ restCall (R.CreateMessageDetailed (messageChannelId m) opts)
        _ -> return ()



notFromBot :: Message -> Bool
notFromBot m = not $ userIsBot (messageAuthor m)


isCommand :: Message -> Bool
isCommand m = "Hasbot, please " `T.isPrefixOf` (messageContent m)


parseCommand :: Message -> IO String
parseCommand m = do
  let msg = T.unpack $ messageContent m
  let msgParts = words msg
  let command = drop 2 msgParts -- Get rid of "Hasbot," and "please"
  if null command
    then return "Error: You need to tell Hasbot what to do."
    else case head command of
      "hoogle" -> if length command /= 5
                  then return "`ERROR`: Wrong usage of Hoogle command.\n Try it like this: `Hasbot, please hoogle 2 docs for map`\n You can also ask Hasbot for help: `Hasbot, please help`"
                  else case command !! 2 of
                         "docs"       -> hoogleDocs (command !! 4) (command !! 1) -- Example command: hoogle 2 docs for map
                         "signatures" -> hoogleSignatures (command !! 4) (command !! 1)
      "help"   -> return produceBotResponseForHelpCommand
      _        -> return "`ERROR`: Not a valid command.\nYou can ask Hasbot for help: `Hasbot, please help`"
