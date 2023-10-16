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
        MessageCreate m -> when (isCommand m) $ do

          hoogleResponses <- liftIO $ queryHoogleAPIFor "map" "2"
          let importantInfos = getImportantInfoAboutAllImplementations hoogleResponses
          let formattedInfos = map (\(p, m, s) -> T.pack $ "```json" ++ "\n \
                                                           \ \"Package\": " ++ p ++ "\n \
                                                           \ \"Module\": " ++ m ++ "\n \
                                                           \ \"Signature\": " ++ s ++ "```"
                                   ) importantInfos
          liftIO $ mapM_ print formattedInfos


          void $ restCall (R.CreateMessage (messageChannelId m) (head formattedInfos))
        _ -> return ()



-- TODO: Monadic function chains ?
parseMessage :: Message -> IO ()
parseMessage m = when (isCommand m) $ do
  let msg = T.unpack $ messageContent m
  let cmdParts = tail $ words msg
  let response = if head cmdParts /= "please" then return "You have to be more polite to Hasbot." else tail cmdParts
  print response




notFromBot :: Message -> Bool
notFromBot m = not $ userIsBot (messageAuthor m)

isCommand :: Message -> Bool
-- isCommand m = "/" `isPrefixOf` (messageContent m)
isCommand m = "Hasbot, " `T.isPrefixOf` (messageContent m)
