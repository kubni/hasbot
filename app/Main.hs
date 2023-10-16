{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad (when, void)
import           Control.Monad.IO.Class (liftIO)
import           UnliftIO.Concurrent
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import           Discord
import           Discord.Types
import qualified Discord.Requests as R

import Data.Aeson.Encoding


import HoogleCommands

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
          void $ restCall (R.CreateMessage (messageChannelId m) (T.pack botResponse))
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

  case head command of
    "hoogle" -> produceBotResponseForHoogleCommand (command !! 1) (command !! 2)
    "help"   -> return produceBotResponseForHelpCommand
    _        -> return "Error, not a valid command"
