module HasbotCommands (
  module HoogleCommands,
  produceBotResponseForHelpCommand
)
where

import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map



-- The more complicated commands will have their own modules.
import HoogleCommands



getAllHasbotCommandsAndTheirDescription :: Map String String
getAllHasbotCommandsAndTheirDescription = Map.fromList
  [
    ("∘` hoogle`", "-- Search the Haskell API.\n Usage: `Hasbot, please hoogle <number_of> <docs|signatures> for <function_name>`"),
    ("∘` help`", "-- Print all available commands and their short descriptions.\n Usage: `Hasbot, please help`")
  ]


produceBotResponseForHelpCommand :: String
produceBotResponseForHelpCommand = do
  let l = map (\(k, v) -> k ++ " " ++ v) (Map.toList getAllHasbotCommandsAndTheirDescription)
  "Available commands: \n" ++ intercalate "\n" l
