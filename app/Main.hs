{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad (when, void)
import           Control.Monad.IO.Class (liftIO)
import           UnliftIO.Concurrent
import           Data.Text (isPrefixOf, toLower, Text, unpack)
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
        MessageCreate m -> when (isCommand m) $ do
          liftIO $ queryHoogleAPIFor "map"
          -- liftIO $ parseMessage m
          void $ restCall (R.CreateMessage (messageChannelId m) "Pong!")
        _ -> return ()



-- TODO: Monadic function chains ?
parseMessage :: Message -> IO ()
parseMessage m = when (isCommand m) $ do
  let msg = unpack $ messageContent m
  let cmdParts = tail $ words msg
  let response = if head cmdParts /= "please" then return "You have to be more polite to Hasbot." else tail cmdParts
  print response




notFromBot :: Message -> Bool
notFromBot m = not $ userIsBot (messageAuthor m)

isCommand :: Message -> Bool
-- isCommand m = "/" `isPrefixOf` (messageContent m)
isCommand m = "Hasbot, " `isPrefixOf` (messageContent m)
